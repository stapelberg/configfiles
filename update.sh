#!/usr/bin/zsh
# vim:ts=4:sw=4:expandtab
#
# © 2011-2013 Michael Stapelberg, Public Domain

cfgfiles=$(dirname $(readlink ~/.zshrc))

fail_update() {
    echo "$1" >&2
    # If this file exists, there is a nagging warning in the zsh prompt.
    touch "$cfgfiles/ERROR"
    exit 1
}

# Ensure only one instance of this script is running (mkdir is atomic).
if ! mkdir "$cfgfiles/update_lock" 2>/dev/null; then
    # Two things could’ve happened:
    # 1) Another instance of the script is running.
    # 2) $cfgfiles is not writable (e.g. a user ran initialize.sh from another
    #    user’s checkout, or the filesystem is read-only).
    # To catch the second case, we try to create a temporary directory.
    tempdir=$(mktemp -d --tmpdir="$cfgfiles" 2>/dev/null)
    [[ $? -ne 0 ]] && fail_update "$cfgfiles is not writable"
    rm -r "$tempdir"

    # By now, case 2 is handled, so another instance of the script must be
    # running.
    exit 0
fi

# Cleanup the lock on exit/signal
trap 'rm -rf "$cfgfiles/update_lock"' 0

# XXX: In the future, the following lines could be run in background.
echo "NOTE: updating git for $cfgfiles"

# use e.g. ~/.configfiles.TMP as temporary copy to perform the update in
new_location=$(dirname "$cfgfiles")/.$(basename "$cfgfiles").TMP
[ -d "$new_location" ] && fail_update "new_location $new_location already exists"
mkdir "$new_location" || fail_update "new_location $new_location could not be created"

# Copy all the files from the current directory to the new location
(cd "$cfgfiles" >/dev/null && tar cp .) | tar x --directory "$new_location"

(
    cd "$new_location" >/dev/null

    echo "--- git stash ---"
    git stash || fail_update "could not “git stash” the current changes"

    echo "--- git pull ---"
    git pull --ff-only || fail_update "new changes could not be applied"

    echo "--- git stash apply ---"
    git stash apply || fail_update "could not apply old changes with “git stash apply”"
) >$cfgfiles/last-update.log 2>&1

# Move files from the updated copy to the original folder.
IFS=$'\n'
for file in $(cd "$new_location" >/dev/null && git ls-files); do
    mv "$new_location/$file" "$cfgfiles/$file" || fail_update "could not move “$file”"
done

# Copy .git to the original folder
(cd "$new_location" >/dev/null && tar cp .git) | tar x --directory "$cfgfiles"

rm -rf "$new_location"
