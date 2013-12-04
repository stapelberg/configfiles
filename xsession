#!/bin/sh

# Enable zapping (C-A-<Bksp> kills X)
setxkbmap -option terminate:ctrl_alt_bksp

# Run redshift, but don’t fail if it’s not (yet) available
if which redshift >/dev/null 2>&1; then
	redshift -m randr -l 47.31:8.50 -t 6000:5300 -b 0.8 &
fi

HOSTNAME=$(hostname)
if [ "$HOSTNAME" = "midna" ]; then
	# Slightly modified Xmodmap for the Kinesis keyboard
	setxkbmap lv && xmodmap .Xmodmap

	# Configure monitors
	xrandr --output HDMI2 --mode 1920x1200 --right-of HDMI1

	#~/notify-printer-status/notify-printer-status-loop.sh &
else
	setxkbmap lv && xmodmap .Xmodmap_us_neo && xset -r 51
fi
# Enable mousekeys to have a paste button
xkbset m
xkbset exp '=m'
if [ "$HOSTNAME" = "x200" ]
then
	# Enable scrolling on thinkpad
	xinput set-prop 'TPPS/2 IBM TrackPoint' "Evdev Wheel Emulation Button" 2
	xinput set-prop 'TPPS/2 IBM TrackPoint' "Evdev Wheel Emulation Axes" 6 7 4 5
	xinput set-prop 'TPPS/2 IBM TrackPoint' "Evdev Wheel Emulation" 1
fi

# needed for toggle_layout
touch /tmp/neo_active

# Allow local connections from all users, so that when using 'sudo vim' we can
# get access to the X clipboard.
xhost +local:

# X settings: disable DPMS, disable audible bell
xset dpms force on s off -b

# Set correct locales
unset LC_ALL
export LANG=de_DE.UTF-8
export LC_MESSAGES=C
export LC_TIME=en_DK.UTF-8

# set PAGER/EDITOR for things not started by a shell
export PAGER=less
export EDITOR=vim

export GTK_IM_MODULE=xim

# Background color
xsetroot -solid "#333333" -cursor_name left_ptr

# Start urxvtd
urxvtd -q -f -o

#cd /tmp
#rm xsession.log
#export DISPLAY=:0
#xtrace -o xsession.log &
#sleep 1
#export DISPLAY=:9
#cd /home/michael

# Start i3, my compiled version if available, fallback to the system version
# otherwise.
if [ -x ~/i3/i3 ]; then
	exec ~/i3/i3
else
	exec i3
fi
