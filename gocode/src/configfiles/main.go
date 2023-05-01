package main

import (
	"flag"
	"fmt"
	"io"
	"io/ioutil"
	"log"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strconv"
	"strings"
	"time"

	"github.com/godbus/dbus"
	"github.com/nightlyone/lockfile"
)

var (
	force = flag.Bool("force",
		false,
		"Force an update, even though local/remote are at the same revision and/or the config files were recently updated.")

	quiet = flag.Bool("quiet",
		false,
		"Do not print any log messages to stderr.")

	daemonMode = flag.Bool("daemon",
		false,
		"If true, will run in an endless loop, updating config files once an hour and whenever network connectivity (as communicated by NetworkManager) changes.")

	configfilesDir = flag.String("configfiles_dir",
		"/home/michael/configfiles",
		"Path to the configuration files working copy")

	readOnlyGitRemote = flag.String("read_only_git_remote",
		"git://code.stapelberg.de/configfiles",
		"git:// URL to the config file repository (read-only)")
)

// From libnm-util/NetworkManager.h
const (
	NM_DEVICE_STATE_ACTIVATED = 100
)

func filterStateChanges(in <-chan *dbus.Signal, out chan<- bool) {
	for signal := range in {
		if len(signal.Body) != 3 {
			log.Panicf("protocol error: NetworkManager sent a StateChanged signal with %d members, expected 3\n", len(signal.Body))
		}
		newState, ok := signal.Body[0].(uint32)
		if !ok {
			log.Panicf("protocol error: NetworkManager sent a StateChanged signal where members are not uint32\n")
		}
		if newState != NM_DEVICE_STATE_ACTIVATED {
			continue
		}
		// We don’t use NetworkManager’s CheckConnectivity method because it
		// has many false-positives (at least with NetworkManager 1.0.6), i.e.
		// it will say you have full connectivity, even though you are
		// connected to an Android tethering hotspot without upstream
		// connectivity. So, we save the code complexity of dealing with an API
		// that doesn’t provide us useful data.

		// Trigger a check if we can. If we can’t, the implication is that a
		// check is currently running, which is fine as well.
		select {
		case out <- true:
		default:
		}
	}
}

func daemon() {
	conn, err := dbus.SystemBus()
	if err != nil {
		log.Fatal(err)
	}

	// Subscribe to device state changes so that we get notified when a device
	// gets activated. We can then trigger a connectivity check.
	if err := conn.BusObject().Call(
		"org.freedesktop.DBus.AddMatch",
		0,
		"type='signal',"+
			"interface='org.freedesktop.NetworkManager.Device',"+
			"member='StateChanged'").Err; err != nil {
		log.Fatal(err)
	}

	connectivityChanged := make(chan bool)

	// If filterStateChanges does not read signals fast enough, i.e. the signal
	// channel write blocks, godbus will panic. The intention of the channel
	// buffer (which was arbitrarily sized) is to make that situation as
	// unlikely as possible.
	signals := make(chan *dbus.Signal, 100)
	conn.Signal(signals)
	go filterStateChanges(signals, connectivityChanged)

	for {
		select {
		case <-connectivityChanged:
			log.Printf("Connectivity changed. Waiting for the situation to settle (1m without changes).\n")

		Settled:
			for {
				select {
				case <-connectivityChanged:
					log.Printf("Connectivity changed again, waiting another 1m.\n")
				case <-time.After(1 * time.Minute):
					break Settled
				}
			}

			log.Printf("could check for an update now\n")
			if err := pull(); err != nil {
				log.Fatal(err)
			}

		case <-time.After(1 * time.Hour):
			log.Printf("check opportunistically for an update now\n")
			if err := pull(); err != nil {
				log.Fatal(err)
			}
		}
	}
}

func copyFile(src, dest string) error {
	in, err := os.Open(src)
	if err != nil {
		return err
	}
	defer in.Close()
	fi, err := in.Stat()
	if err != nil {
		return err
	}
	// Remove the destination file in case it is read-only (packfiles of git
	// submodules are read-only).
	if err := os.Remove(dest); err != nil && !os.IsNotExist(err) {
		return err
	}
	out, err := os.OpenFile(dest, os.O_CREATE|os.O_RDWR, fi.Mode())
	if err != nil {
		return err
	}
	if _, err := io.Copy(out, in); err != nil {
		return err
	}
	if err := out.Close(); err != nil {
		return nil
	}
	return os.Chtimes(dest, time.Now(), fi.ModTime())
}

func copyAll(src, dest string) error {
	return filepath.Walk(src, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		if info.IsDir() {
			return os.MkdirAll(strings.Replace(path, src, dest, 1), info.Mode())
		}
		if !info.Mode().IsRegular() {
			return nil
		}
		return copyFile(path, strings.Replace(path, src, dest, 1))
	})
}

func gitTrackedFiles(dir string, stderr io.Writer) (map[string]bool, error) {
	cmd := exec.Command("git", "ls-files")
	cmd.Dir = dir
	cmd.Stderr = stderr
	out, err := cmd.Output()
	if err != nil {
		return nil, err
	}
	tracked := make(map[string]bool)
	for _, filename := range strings.Split(string(out), "\n") {
		if strings.TrimSpace(filename) == "" {
			continue
		}
		tracked[filename] = true
	}
	return tracked, nil
}

func copyGitTracked(src, dest string) error {
	tracked, err := gitTrackedFiles(src, ioutil.Discard)
	if err != nil {
		return err
	}
	return filepath.Walk(src, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		relative := strings.Replace(path, src+"/", "", 1)
		if !tracked[relative] {
			return nil
		}
		if info.IsDir() {
			return os.MkdirAll(strings.Replace(path, src, dest, 1), info.Mode())
		}
		if !info.Mode().IsRegular() {
			return nil
		}

		destfn := strings.Replace(path, src, dest, 1)
		if err := os.MkdirAll(filepath.Dir(destfn), 0755); err != nil {
			return err
		}

		return copyFile(path, destfn)
	})
}

func moveGitFiles(src, dest string, errLog io.Writer) error {
	tracked, err := gitTrackedFiles(dest, errLog)
	if err != nil {
		return err
	}
	return filepath.Walk(src, func(path string, info os.FileInfo, err error) error {
		if info.IsDir() {
			return os.MkdirAll(strings.Replace(path, src, dest, 1), info.Mode())
		}
		if !info.Mode().IsRegular() {
			return nil
		}
		relative := strings.Replace(path, src+"/", "", 1)
		if !tracked[relative] {
			return nil
		}
		return os.Rename(path, dest+"/"+relative)
	})

}

func judgeErrors() error {
	logs, err := os.Open(filepath.Join(*configfilesDir, ".logs"))
	if err != nil {
		return err
	}
	defer logs.Close()

	names, err := logs.Readdirnames(-1)
	if err != nil {
		return err
	}

	sort.Sort(sort.Reverse(sort.StringSlice(names)))

	seenSuccess := false
	for _, name := range names {
		if !seenSuccess && strings.HasSuffix(name, ".success") {
			seenSuccess = true
		}
		last := strings.LastIndex(name, ".")
		if last == -1 {
			last = len(name)
		}
		epoch, err := strconv.ParseInt(name[:last], 0, 64)
		if err != nil {
			return err
		}
		if time.Since(time.Unix(epoch, 0)) > 5*time.Hour {
			break
		}
	}
	if !seenSuccess {
		log.Printf("No success within the last 5 hours, creating ERROR file\n")
		f, err := os.Create(filepath.Join(*configfilesDir, "ERROR"))
		if err != nil {
			return err
		}
		return f.Close()
	}
	return nil
}

func pullStashed(twd string, errLog io.Writer) error {
	cmd := exec.Command("git", "stash")
	cmd.Dir = twd
	cmd.Stdout = errLog
	cmd.Stderr = errLog
	if err := cmd.Run(); err != nil {
		return err
	}

	cmd = exec.Command("git", "pull", "--ff-only")
	cmd.Dir = twd
	cmd.Stdout = errLog
	cmd.Stderr = errLog
	if err := cmd.Run(); err != nil {
		return err
	}

	// Check if there are stashed changes at all, since “git stash pop” returns
	// with non-zero exit code when called with no stashed changes.
	cmd = exec.Command("git", "stash", "list")
	cmd.Dir = twd
	out, err := cmd.Output()
	if err != nil {
		return err
	}
	if strings.TrimSpace(string(out)) == "" {
		return nil
	}

	cmd = exec.Command("git", "stash", "pop")
	cmd.Dir = twd
	cmd.Stdout = errLog
	cmd.Stderr = errLog
	return cmd.Run()
}

func pull() error {
	logTime := strconv.FormatInt(time.Now().Unix(), 10)
	gitDir := filepath.Join(*configfilesDir, ".git")
	fi, err := os.Stat(gitDir)
	if err != nil {
		return fmt.Errorf("cannot stat %q: %v\n", err)
	}
	if !*force && time.Since(fi.ModTime()) < 1*time.Hour {
		log.Printf("%q was modified within the last hour, skipping update\n", gitDir)
		return nil
	}

	// Skip update if the remote git points to the same commit as the local
	// repository.
	log.Printf("Resolving HEAD of %q\n", *readOnlyGitRemote)
	remoteHead, err := gitResolve(*readOnlyGitRemote)
	if err != nil {
		log.Printf("Skipping this update, as resolving git HEAD failed: %v\n", err)
		f, err := os.Create(filepath.Join(*configfilesDir, ".logs", logTime+".skipped"))
		if err != nil {
			return err
		}
		return f.Close()
	}

	cmd := exec.Command("git", "log", "--format=%H", "--max-count=1")
	cmd.Dir = *configfilesDir
	out, err := cmd.Output()
	if err != nil {
		return fmt.Errorf("Could not run git log: %v\n", err)
	}
	if !*force && strings.TrimSpace(string(out)) == remoteHead {
		log.Printf("Already up-to-date (both local and remote are at %s)\n", remoteHead)
		return nil
	}

	// Lock to guard against multiple updater processes running at the same
	// time, potentially stepping on eath other’s toes.
	lockPath := filepath.Join(*configfilesDir, "update_lock")
	lock, err := lockfile.New(lockPath)
	if err != nil {
		return err
	}
	if err := lock.TryLock(); err != nil {
		return fmt.Errorf("Could not lock %q: %v", lockPath, err)
	}
	defer lock.Unlock()

	// Copy configfiles to a temporary working directory (|twd|), so that we
	// can update it without overwriting configfiles.
	abs, err := filepath.Abs(*configfilesDir)
	if err != nil {
		return err
	}
	twd := filepath.Join(filepath.Dir(abs), "."+filepath.Base(abs)+".TMP")

	log.Printf("Cloning configfiles from %q to %q\n", abs, twd)

	if err := os.RemoveAll(twd); err != nil {
		return err
	}
	if err := os.MkdirAll(filepath.Dir(abs), 0755); err != nil {
		return err
	}

	if err := copyGitTracked(abs, twd); err != nil {
		return err
	}
	if err := copyAll(filepath.Join(abs, ".git"), filepath.Join(twd, ".git")); err != nil {
		return err
	}

	if err := os.MkdirAll(filepath.Join(*configfilesDir, ".logs"), 0755); err != nil {
		return err
	}

	errLogPath := filepath.Join(*configfilesDir, ".logs", logTime+".failure")
	errLog, err := os.Create(errLogPath)
	if err != nil {
		return err
	}
	defer errLog.Close()

	log.Printf("Pulling new changes via git\n")

	if err := pullStashed(twd, errLog); err != nil {
		return err
	}

	log.Printf("Moving new files to %q\n", *configfilesDir)

	if err := moveGitFiles(twd, *configfilesDir, errLog); err != nil {
		return err
	}

	successLogPath := errLogPath[:len(errLogPath)-len(".failure")] + ".success"
	if err := os.Rename(errLogPath, successLogPath); err != nil {
		return err
	}

	log.Printf("Update done successfully\n")

	if err := copyAll(filepath.Join(twd, ".git"), filepath.Join(*configfilesDir, ".git")); err != nil {
		return err
	}

	return judgeErrors()
}

func main() {
	flag.Parse()

	if *quiet {
		os.Stderr.Close()
	}

	if *daemonMode {
		daemon()
	}

	if err := pull(); err != nil {
		log.Fatal(err)
	}
}
