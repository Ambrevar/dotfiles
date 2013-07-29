#!/bin/sh

if [ $(tput colors) -eq 256 ]; then
    ## 256 colors
    cat <<EOF
set my_address = color69
set my_from = color201
set my_to = color39
set my_cc = green
set my_bcc = green
set my_replyto = green
set my_subject = brightred
set my_inreplyto = green
set my_date = yellow
set my_x = blue
set my_sig = brightblack
set my_attach = white
set my_attach_bg = black
set my_status = brightwhite
set my_status_bg = color240
set my_indicator = white
set my_indicator_bg = color69

set my_quote0 = color28
set my_quote1 = color29
set my_quote2 = color30
set my_quote3 = color31
set my_quote4 = color32
set my_quote5 = color33
set my_quote6 = color39
set my_quote7 = color38
EOF

else
    ## 8/16 colors
    cat <<EOF
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
