#!/bin/sh
################################################################################
## XINITRC
## 2012-10-29
################################################################################

## Config (for URxvt)
[ -f "${HOME}/.Xresources" ] && xrdb -merge "${HOME}/.Xresources"

## Useful tools
numlockx on

 if [ "$OSTYPE" = "linux-gnu" ] ; then
    [ -n "$(amixer | grep PCM)" ] && amixer set PCM 80%
    udiskie &
fi

## Environment
[ -f ~/.shell.d/main_rc ] && . ~/.shell.d/main_rc

##==============================================================================
## Launch session
##==============================================================================
exec awesome

## Crappy desktops
# exec razor-session
# exec startkde
# exec startlxde
# exec gnome-session

##==============================================================================
## Wallpaper
##==============================================================================
# feh --bg-fill "$HOME/pictures/arch_linux_wallpaper_by_mokkujin-d3j1se0.png"
