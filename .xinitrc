#!/bin/sh
################################################################################
## XINITRC
## Last updated: 2012-04-23
################################################################################

## Config (for rxvt)
[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

## Useful tools
numlockx on
[ "$(amixer | grep PCM)" != "" ] && amixer set PCM 80%
# xscreensaver -no-splash &

##==============================================================================
## My apps
##==============================================================================
# [ -e /usr/bin/emacs ] && emacs --daemon
# dropboxd &
# parcellite &
# clementine &
# thunderbird &

##==============================================================================
## Launch session
##==============================================================================
# [ -e /usr/bin/awesome ] && exec ck-launch-session dbus-launch awesome
[ -e /usr/bin/awesome ] && exec ck-launch-session dbus-launch awesome

# [ -e /usr/bin/startlxde ] && exec ck-launch-session startlxde

# exec ck-launch-session dbus-launch wmfs2
# exec ck-launch-session dbus-launch wmfs
# exec ck-launch-session dbus-launch startkde
# exec ck-launch-session dbus-launch cinnamon 
# exec ck-launch-session gnome-session

## WMII
#exec wmii
#until wmii; do
#	true
#done

################################################################################
## Wallpaper
################################################################################
# feh --bg-fill "$HOME/pictures/arch_linux_wallpaper_by_mokkujin-d3j1se0.png"
