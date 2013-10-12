#!/bin/sh
################################################################################
## XINITRC
## 2013-10-12
################################################################################

## Source environment config.
[ -f ~/.xprofile ] && . ~/.xprofile

##==============================================================================
## Launch session
##==============================================================================

if [ -n "$(command -v $1)" ]; then
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

