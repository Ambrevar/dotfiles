#!/bin/sh
## 8/16 colors

if [ $(tput colors) -ne 256 ]; then
	cat <<EOF
set my_bg = default
set my_new = brightwhite
set my_old = red
set my_uri = green
set my_flag = white

set my_address = brightblue
set my_from = magenta
set my_to = brightblue
set my_cc = green
set my_bcc = green
set my_replyto = green
set my_subject = yellow
set my_inreplyto = red
set my_date = cyan
set my_x = blue
set my_sig = brightblack
set my_attach = white
set my_attach_bg = black
set my_status = black
set my_status_bg = white
set my_indicator = white
set my_indicator_bg = blue

set my_quote0 = brightred
set my_quote1 = brightgreen
set my_quote2 = brightyellow
set my_quote3 = brightblue
set my_quote4 = brightcyan
set my_quote5 = brightmagenta
set my_quote6 = brightwhite
set my_quote7 = green
EOF
fi
