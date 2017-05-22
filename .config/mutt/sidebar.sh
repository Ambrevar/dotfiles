#!/bin/sh

## Set sidebar options only if sidebar is builtin.
if [ -n "$(man muttrc | grep sidebar)" ]; then
	cat <<'EOF'
set sidebar_visible = yes
set sidebar_width = 24
set sidebar_sort_method = alpha
set sidebar_divider_char='│'
set sidebar_short_path = yes
set sidebar_format = "%B%?F? [%F]?%* %?N?%N/?%S"
set sidebar_folder_indent = yes

## Color of folders with new mail
color sidebar_new $my_new $my_bg
color sidebar_flagged white $my_bg
# color sidebar_divider $my_status $my_status_bg
color sidebar_divider $my_flag $my_bg
color sidebar_highlight white $my_status_bg

## Folder selection, Emacs and vi bindings.
bind index,pager \CP sidebar-prev
bind index,pager \CN sidebar-next
bind index,pager \CO sidebar-open
bind index,pager <Esc>k sidebar-prev
bind index,pager <Esc>j sidebar-next
bind index,pager <Esc>l sidebar-open

## Toggle sidebar visibility. Screen might get messed up, hence the refresh.
macro index b '<enter-command>toggle sidebar_visible<enter><refresh>'
macro pager b '<enter-command>toggle sidebar_visible<enter><redraw-screen>'
EOF
fi
