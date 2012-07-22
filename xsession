#!/bin/sh

HOSTNAME=$(hostname)
if [ "$HOSTNAME" = "midna" ]; then
	# Slightly modified Xmodmap for the Kinesis keyboard
	setxkbmap lv && xmodmap .Xmodmap

	# Configure monitors
	xrandr --output HDMI2 --mode 1920x1200 --right-of HDMI1
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

# Enable zapping (C-A-<Bksp> kills X)
setxkbmap -option terminate:ctrl_alt_bksp

# needed for toggle_layout
touch /tmp/neo_active

# X settings: disable DPMS, disable audible bell
xset dpms force on s off -b

# Set correct locales
export LC_CTYPE=de_DE.UTF-8
export LC_TIME=en_DK.UTF-8
export LC_NUMERIC=de_DE.UTF-8
export LC_MONETARY=de_DE.UTF-8
export LC_MESSAGES=C
export LC_PAPER=de_DE.UTF-8
export LC_NAME=de_DE.UTF-8
export LC_ADDRESS=de_DE.UTF-8
export LC_TELEPHONE=de_DE.UTF-8
export LC_MEASUREMENT=de_DE.UTF-8
export LC_IDENTIFICATION=de_DE.UTF-8

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
