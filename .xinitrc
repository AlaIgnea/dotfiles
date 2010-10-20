#!bin/sh
# variables
export PATH="${PATH}:/home/scripts:/usr/local/bin"
export BROWSER="google-chrome"
export LANG="en_US.utf8"
export LC_ALL="en_US.utf8"
export EDITOR="vim"
# dbus
eval `dbus-launch --sh-syntax`
# background
/usr/bin/feh --bg-scale $HOME/.wallpaper.png
# touchpad toggle
/home/scripts/touchpadtest
# set the cursor
xsetroot -cursor_name left_ptr
# window manager
exec xmonad
