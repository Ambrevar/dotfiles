#!/bin/sh
################################################################################
## XINITRC
################################################################################

## Source environment config.
[ -f ~/.xprofile ] && . ~/.xprofile

##==============================================================================
## Launch session
##==============================================================================

if [ $# -eq 1 ] && command -v $1 >/dev/null; then
    exec $1
else
    exec awesome
fi

## Crappy desktops
# exec openbox-session
# exec razor-session
# exec cinnamon-session
# exec startkde
# exec startlxde
# exec gnome-session

