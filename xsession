#!/usr/bin/env bash

# Vanilla xdm does not do this by default:
xrdb $HOME/.Xresources

# Enable zapping (C-A-<Bksp> kills X)
# Using setxkbmap -option terminate:ctrl_alt_bksp binds NoSymbol to Backspace
# on the higher levels so that CapsLock+Backspace (Backspace on Level3) doesn’t
# work with the NEO layout.
xmodmap -e 'keycode 22 = BackSpace BackSpace BackSpace BackSpace BackSpace BackSpace Terminate_Server BackSpace BackSpace Terminate_Server'

# Run redshift, but don’t fail if it’s not (yet) available
if command -V redshift >/dev/null 2>&1; then
	redshift -m randr -l 47.31:8.50 -t 6000:5300 -b 0.8 &
fi

xrandr --dpi 192̂

INPUT_DEVICES=$(xinput list --name-only)
TRACKBALL="Logitech M570"
TRACKPOINT="TPPS/2 IBM TrackPoint"
TRACKPOINT_ELAN="TPPS/2 Elan TrackPoint"
TOUCHPAD="SynPS/2 Synaptics TouchPad"
DELLTOUCHPAD="DLL082A:01 06CB:76AF Touchpad"

if [[ "$HOST" = "midna.zekjur.net" || "$HOSTNAME" = "midna.zekjur.net" || "$HOSTNAME" = "midna" ]]; then
	HOST="midna"
fi

if [[ "$HOSTNAME" == pacna.* ]]
then
	xrandr --output DP-5.8 --auto --right-of DP-5.9
	xrandr --setmonitor up2414q auto DP-5.8,DP-5.9
fi

if [[ "$HOST" = "midna" ]]
then
	xrandr --setmonitor up3218k auto DP-4,DP-2
fi

setxkbmap de -variant neo

if echo "$INPUT_DEVICES" | grep -qF "$TRACKBALL"; then
	xinput set-ptr-feedback "$TRACKBALL" 10 16 15
	xinput set-prop "$TRACKBALL" 'Device Accel Profile' 2
	xinput set-prop "$TRACKBALL" 'Device Accel Adaptive Deceleration' 1.2
	# multiply cursor movement by 2x for hidpi
	xinput set-prop "$TRACKBALL" "Device Accel Constant Deceleration" 0.5
fi

if echo "$INPUT_DEVICES" | grep -qF "$TRACKPOINT"; then
	# Enable scrolling on thinkpad
	xinput set-prop "$TRACKPOINT" "Evdev Wheel Emulation Button" 2
	xinput set-prop "$TRACKPOINT" "Evdev Wheel Emulation Axes" 6 7 4 5
	xinput set-prop "$TRACKPOINT" "Evdev Wheel Emulation" 1
fi

if echo "$INPUT_DEVICES" | grep -qF "$TRACKPOINT_ELAN"; then
    xinput set-prop "TPPS/2 Elan TrackPoint" "Evdev Wheel Emulation" 1
    xinput set-prop "TPPS/2 Elan TrackPoint" "Evdev Wheel Emulation Button" 2
    xinput set-prop "TPPS/2 Elan TrackPoint" "Evdev Wheel Emulation Timeout" 200
    xinput set-prop "TPPS/2 Elan TrackPoint" "Evdev Wheel Emulation Axes" 6 7 4 5
fi

# xps 13 9360
if echo "$INPUT_DEVICES" | grep -qF "SynPS/2 Synaptics TouchPad"; then
	synclient TapButton1=1
	synclient TapButton2=3
	synclient TapButton3=2
	synclient VertScrollDelta=-113
	synclient HorizScrollDelta=-113
fi

# ThinkPad X1 Extreme
if echo "$INPUT_DEVICES" | grep -qF "Synaptics TM3512-010"; then
    xinput set-prop "Synaptics TM3512-010" "libinput Natural Scrolling Enabled" 1
fi

if echo "$INPUT_DEVICES" | grep -qF "$DELLTOUCHPAD"; then
      synclient TapButton3=2
      synclient VertScrollDelta=-27
      synclient HorizScrollDelta=-27
else
	if echo "$INPUT_DEVICES" | grep -qF "$TOUCHPAD"; then
		# Older Xorg setups:
		synclient VertScrollDelta=-150
		synclient HorizScrollDelta=-150
		synclient VertTwoFingerScroll=1
		synclient HorizTwoFingerScroll=1

		# libinput-based Xorg setups:
		xinput set-prop "$TOUCHPAD" 'libinput Natural Scrolling Enabled' 1
	fi
fi

# needed for toggle_layout
touch /tmp/neo_active

# Allow local connections from all users, so that when using 'sudo vim' we can
# get access to the X clipboard.
xhost +local:

# X settings: disable DPMS, disable audible bell
xset -dpms s off -b

# increase key repeat rate
xset r rate 250 30

# Set correct locales
unset LC_ALL
export LANG=de_DE.UTF-8
export LC_MESSAGES=C
export LC_TIME=en_DK.UTF-8

# set PAGER/EDITOR for things not started by a shell
export PAGER=less
export EDITOR=vim

# # Make Java SWT use GTK3, which has better hidpi support:
# export SWT_GTK3=1

# Needed for GTK3 programs like Evince, Geeqie, GIMP 3, … to scale correctly:
#
# NOTE: Chrome 131+ scales incorrectly when GDK_SCALE is set:
# https://mas.to/@zekjur/113475631028231867
# …but that needs to be worked around in Chrome,
# e.g. by using --force-device-scale-factor=3 in ~/.config/chrome-flags.conf
export GDK_SCALE=2
export GDK_DPI_SCALE=0.5
# Needed for some Qt programs like Wireshark to scale correctly:
# The default depends on whether the program was updated apparently.
# Most programs I tried behaved as if QT_AUTO_SCREEN_SCALE_FACTOR=1,
# but not Wireshark.
export QT_AUTO_SCREEN_SCALE_FACTOR=1

# Background color
xsetroot -solid "#333333" -cursor_name left_ptr

# Start urxvtd
urxvt256c-mld -q -f -o || urxvtd -q -f -o

#cd /tmp
#rm xsession.log
#export DISPLAY=:0
#xtrace -o xsession.log &
#sleep 1
#export DISPLAY=:9
#cd /home/michael

# Fedora’s Xsession ends up in /, not in $HOME
cd $HOME

# Remove comment to prefer the working directory’s compiled version:
#[ -x ~/i3/build/i3 ] && exec ~/i3/build/i3

# Restart i3 if killed so that the session might survive.
exec i3
