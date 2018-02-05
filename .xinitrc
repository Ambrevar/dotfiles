#!/bin/sh

## WARNING: If started from TTY on a multi-seat, the user must run 'startx &
## vlock' to ensure the X locker cannot be bypassed.

## Run site init scripts. Usually not necessary.
if [ -d /etc/X11/xinit/xinitrc.d ] ; then
	for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
		[ -x "$f" ] && . "$f"
	done
	unset f
fi

## Source environment config.
[ -f ~/.xprofile ] && . ~/.xprofile

## Launch session
if [ $# -eq 1 ] && command -v $1 >/dev/null 2>&1; then
	exec $1
else
	conky | dzen2 -p -dock -ta l -fn "DejaVu Sans Mono" &
	## `exwm-enable' has to be called before the frame is spawned.
	emacs --daemon -f exwm-enable
	exec emacsclient -c
fi
