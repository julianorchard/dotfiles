#!/bin/bash

# Systemctl Fix
systemctl --user set-environment DISPLAY=:0

setxkbmap -layout gb

xrdb -merge "$HOME"/.config/X11/Xresources

if [ -d /etc/X11/xinit/xinitrc.d ]; then
    for f in /etc/X11/xinit/xinitrc.d/?*.sh; do
        # If $f executable, execute
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

# Whichever window manager you want!
exec dbus-run-session i3
# exec gnome-session
