#!/bin/sh
################################################################################
## XINITRC
## Last updated: 2012-07-30
################################################################################

## Config (for URxvt)
[ -f "${HOME}/.Xresources" ] && xrdb -merge "${HOME}/.Xresources"

## Useful tools
numlockx on
[ "$(amixer | grep PCM)" != "" ] && amixer set PCM 80%

##==============================================================================
## Launch session
##==============================================================================
udiskie &
exec awesome

# exec startkde

# exec ck-launch-session startlxde
# exec ck-launch-session gnome-session

##==============================================================================
## Wallpaper
##==============================================================================
# feh --bg-fill "$HOME/pictures/arch_linux_wallpaper_by_mokkujin-d3j1se0.png"
