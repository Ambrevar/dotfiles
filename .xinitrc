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
	## `exwm-enable' has to be called before the frame is spawned.
	emacs --daemon -f exwm-enable
	exec emacsclient -c
fi

## TODO: When the session is terminated, some forked processes must be closed
## manually. This can automated by setting a shell 'trap' in .profile when
## logged from a TTY. The trick does not work from an X login manager. This file
## could be the place where to clean everything, but it will not be parsed by
## some login managers. Beside, systemd sessions may sigkill all user apps on
## logoff, preventing a clean termination.
# ssh-agent -k
## See if exec'ing "ssh-agent <WM>" is a better idea.
