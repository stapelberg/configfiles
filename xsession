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
	xrandr --setmonitor up3218k auto DP-4,DP-0
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

# xps 13 9360
if echo "$INPUT_DEVICES" | grep -qF "SynPS/2 Synaptics TouchPad"; then
	synclient TapButton1=1
	synclient TapButton2=3
	synclient TapButton3=2
	synclient VertScrollDelta=-113
	synclient HorizScrollDelta=-113
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

# Disable powersaving on nvidia cards, it causes really bad graphics
# performance, at least with a minimalist window manager such as i3.
if lspci -n | grep -q '\b10de:'; then
	nvidia-settings -a "[gpu:0]/GPUPowerMizerMode=1"
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

export GTK_IM_MODULE=xim

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

# Start i3, my compiled version if available, fallback to the system version
# otherwise.
[ -x ~/i3/build/i3 ] && exec ~/i3/build/i3
[ -x ~/i3/i3 ] && exec ~/i3/i3
exec i3
