package main

import (
	"bufio"
	"fmt"
	"net"
	"net/url"
	"strings"
	"time"
)

// Resolves the revision to which HEAD of the specified git URL points to.
// This function can be used to skip executing git pull (which is typically
// more expensive due to involving SSH or HTTPS).
//
// For details an the git pack protocol, see
// https://github.com/git/git/blob/master/Documentation/technical/pack-protocol.txt
func gitResolve(giturl string) (string, error) {
	u, err := url.Parse(giturl)
	if err != nil {
		return "", err
	}

	// Add git port unless |giturl| already contained a port.
	if _, _, err := net.SplitHostPort(u.Host); err != nil {
		u.Host = u.Host + ":git"
	}

	conn, err := net.Dial("tcp", u.Host)
	if err != nil {
		return "", err
	}
	defer conn.Close()
	conn.SetDeadline(time.Now().Add(1 * time.Minute))
	req := fmt.Sprintf("%04xgit-upload-pack %s\x00host=%s\x00", 0, u.Path, u.Host)
	fmt.Fprintf(conn, "%04x%s", len(req), req[4:])

	suffix := " refs/heads/master\n"

	r := bufio.NewReader(conn)
	// Skip the first line, which is separated by a 0-byte.
	if _, err := r.ReadString('\x00'); err != nil {
		return "", err
	}
	for {
		line, err := r.ReadString('\n')
		if err != nil {
			return "", err
		}
		if strings.HasSuffix(line, suffix) {
			// The first 4 bytes are the length of the line.
			return line[4 : len(line)-len(suffix)], nil
		}
		if strings.HasPrefix(line, "0000") {
			break
		}
	}
	return "", fmt.Errorf("git protocol error: no line ended in %q", suffix)
}
