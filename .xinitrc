#!/bin/sh
################################################################################
## XINITRC
################################################################################

## Source environment config.
[ -f ~/.xprofile ] && . ~/.xprofile

##==============================================================================
## Launch session
##==============================================================================

## Note: we do not 'exec' as we want to clean the session when the session is
## exited.
if [ $# -eq 1 ] && command -v $1 >/dev/null; then
    $1
else
    awesome
fi

command -v sessionclean >/dev/null 2>&1 && sessionclean

## Crappy desktops
# exec openbox-session
# exec razor-session
# exec cinnamon-session
# exec startkde
# exec startlxde
# exec gnome-session

