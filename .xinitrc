#!/bin/sh
# dbus
eval `dbus-launch --sh-syntax`
# background
/usr/bin/feh --bg-scale $HOME/.louise_francoise_et_shana_et_nagi.jpg
# touchpad toggle
/home/scripts/touchpadtest
# set the cursor
xsetroot -cursor_name left_ptr
# X compositing manager
xcompmgr -c &
# window manager
exec xmonad
