#!/bin/sh

# Enable zapping (C-A-<Bksp> kills X)
setxkbmap -option terminate:ctrl_alt_bksp

xmodmap .Xmodmap
# Enable mousekeys to have a paste button
xkbset m
xkbset exp =m
if [ "$(hostname)" = "x200" ]
then
	xmodmap .Xmodmap_us_neo

	# Enable scrolling on thinkpad
	xinput set-prop '"TPPS/2 IBM TrackPoint"' "Evdev Wheel Emulation Button" 2
	xinput set-prop '"TPPS/2 IBM TrackPoint"' "Evdev Wheel Emulation Axes" 6 7 4 5
	xinput set-prop '"TPPS/2 IBM TrackPoint"' "Evdev Wheel Emulation" 1
fi
# needed for toggle_layout
touch /tmp/neo_active

# Disable audible bell
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

#cd /tmp
#rm xtr
#export DISPLAY=:0
#xtrace -o xtr &
#sleep 1
#export DISPLAY=:9
#cd /home/michael

if [ "$(hostname)" = "midna" ]
then
	# Configure monitors
	xrandr --output HDMI2 --mode 1920x1200 --right-of HDMI1
fi

# Initialize part of the logfile
[ ! -d ~/i3 ] && mkdir ~/i3
echo "starting at $(date)" >> ~/i3/i3log
#exec urxvt -geometry 84x24+0+0
#exec twm
exec i3 -V -d all >>~/i3/i3log 2>&1
#exec valgrind -v --log-file=/tmp/vg --leak-check=full --track-origins=yes --num-callers=20 --tool=memcheck i3 -V -d all >>/home/michael/i3/i3log 2>&1
