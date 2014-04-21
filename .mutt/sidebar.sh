#!/bin/sh

## Set sidebar options only if sidebar patch is installed.
if [ -n "$(man muttrc | grep sidebar)" ]; then
    cat <<'EOF'
set sidebar_visible = yes
set sidebar_width = 24
set sidebar_sort = yes
set sidebar_delim=' '
set sidebar_shortpath = yes
# set sidebar_format = "%B%?F? [%F]?%* %?N?%N/?%4S"
# set sidebar_folderindent = yes

## Color of folders with new mail
color sidebar_new $my_new $my_bg

## Ctrl-n, Ctrl-p to select next, previous folder.
## Ctrl-o to open selected folder
bind index,pager \CP sidebar-prev
bind index,pager \CN sidebar-next
bind index,pager \CO sidebar-open

## Toggle sidebar visibility
# macro index b '<enter-command>toggle sidebar_visible<enter>'
# macro pager b '<enter-command>toggle sidebar_visible<enter>'
## Prevent messy output
macro index b '<enter-command>toggle sidebar_visible<enter><refresh>'
macro pager b '<enter-command>toggle sidebar_visible<enter><redraw-screen>'
EOF
fi
