#!/bin/sh

xmodmap .Xmodmap
if [ "$(hostname)" = "x200.zekjur.net" ]
then
	xmodmap .Xmodmap_us_neo
fi
# needed for toggle_layout
touch /tmp/neo_active

# Disable bell
xset -b

# Disable DPMS
xset dpms force on
xset s off

# Set correct locales
export LC_CTYPE=de_DE.utf8
export LC_TIME=de_DE.utf8
export LC_NUMERIC=de_DE.utf8
export LC_MONETARY=de_DE.utf8
export LC_MESSAGES=C
export LC_PAPER=de_DE.utf8
export LC_NAME=de_DE.utf8
export LC_ADDRESS=de_DE.utf8
export LC_TELEPHONE=de_DE.utf8
export LC_MEASUREMENT=de_DE.utf8
export LC_IDENTIFICATION=de_DE.utf8

# Background color
xsetroot -solid "#333333"

# Start urxvtd
urxvtd -q -f -o

# Enable coredumps
ulimit -c unlimited

# Initialize part of the logfile
echo "starting at $(date)" >> /home/michael/i3/i3log
exec i3 >>/home/michael/i3/i3log 2>&1
