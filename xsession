#!/bin/sh

# Enable zapping (C-A-<Bksp> kills X)
setxkbmap -option terminate:ctrl_alt_bksp

# Run redshift, but don’t fail if it’s not (yet) available
if which redshift >/dev/null 2>&1; then
	redshift -m randr -l 47.31:8.50 -t 6000:5300 -b 0.8 &
fi

INPUT_DEVICES=$(xinput list --name-only)
KINESIS="HumbleHacker.com HumbleHacked Kinesis Contoured"
TRACKBALL="Logitech Unifying Device. Wireless PID:1028"
TRACKPOINT="TPPS/2 IBM TrackPoint"

if $HOST = "midna"; then
	# The Dell monitor doesn’t like it.
	xset -dpms
fi

if echo "$INPUT_DEVICES" | grep -qF "$KINESIS"
then
	# Slightly modified Xmodmap for the Kinesis keyboard
	setxkbmap lv && xmodmap .Xmodmap

	# Enable mousekeys to have a paste button
	xkbset m
	xkbset exp '=m'
else
	setxkbmap lv && xmodmap .Xmodmap_us_neo && xset -r 51
fi

if echo "$INPUT_DEVICES" | grep -qF "$TRACKBALL"; then
	xinput set-ptr-feedback "$TRACKBALL" 10 16 15
	xinput set-prop "$TRACKBALL" 'Device Accel Profile' 2
	xinput set-prop "$TRACKBALL" 'Device Accel Adaptive Deceleration' 1.2
fi

if echo "$INPUT_DEVICES" | grep -qF "$TRACKPOINT"; then
	# Enable scrolling on thinkpad
	xinput set-prop 'TPPS/2 IBM TrackPoint' "Evdev Wheel Emulation Button" 2
	xinput set-prop 'TPPS/2 IBM TrackPoint' "Evdev Wheel Emulation Axes" 6 7 4 5
	xinput set-prop 'TPPS/2 IBM TrackPoint' "Evdev Wheel Emulation" 1
fi

# Disable powersaving on nvidia cards, it causes really bad graphics
# performance, at least with a minimalist window manager such as i3.
if lspci -n | grep -q '\b10de:0df8\b'; then
	/usr/lib/nvidia-settings/bin/nvidia-settings.nsw-orig -a "[gpu:0]/GPUPowerMizerMode=1"
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
