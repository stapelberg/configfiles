#!/bin/bash
#
# © 2009 Michael Stapelberg, Public Domain
#
# Creates links for the configuration files in this directory,
# also linking host-specific stuff without the host-prefix itself
# (x200/foo -> .foo) if available.
#
# If a configuration file does not follow the scheme of just being
# named .foorc you can specify custom mappings in the file MAPPINGS.
# For mplayer, the line would look like this:
# mplayer.config ~/.mplayer/config
# Which means that the file called mplayer.config would be put
# into ~/.mplayer/config and ~/.mplayer would be created if necessary

# Get the directory this script lives in
dir="$(cd "${0%/*}" && pwd -P)"

link_dir() {
	for path in $(find $dir/$1 -maxdepth 1 -type f \
		! -regex ".*\(Makefile\|MAPPING\|initialize.sh\|update.sh\)")
	do
		file=$(basename $path)
		# Check if we got a custom mapping
		DEST=$(grep "^$file " $dir/MAPPING | cut -d ' ' -f 2-)
		if [ -z "$DEST" ]
		then
			# Standard location: ~/.filename
			DEST="$HOME/.$file"
		else
			# replace ~ with $HOME
			DEST=${DEST/\~/$HOME}
			# Ensure the target directory exists
			mkdir -p "$(dirname "$DEST")"
		fi

		# Preserve the file, if it already exists
		[[ -e "$DEST" && ! -L "$DEST" ]] && {
			if [ ! -d "$HOME/.configfiles.bak" ]
			then
				mkdir -p "$HOME/.configfiles.bak" || exit 1
			fi
			mv "$DEST" "$HOME/.configfiles.bak/"
		}

		# Skip the file, if it is already a link
		[[ -e "$DEST" && -L "$DEST" ]] && continue

		# Create the symlink
		echo "Linking $DEST"
		ln -s "$dir/$1/$file" "$DEST"
	done
}

[ -d "$(hostname)" ] && link_dir "$(hostname)"
link_dir
