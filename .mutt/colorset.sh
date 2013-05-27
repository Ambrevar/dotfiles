#!/bin/sh
if [ $(tput colors) -eq 256 ]; then
    ## Emacs colors.
    cat <<EOF
set my_col_builtin = color75
set my_col_comment = color242
set my_col_constant = color105
set my_col_doc = color28
set my_col_function = color75
set my_col_keyword = brightred
set my_col_preprocessor = color99
set my_col_string = color39
set my_col_type = color166
set my_col_variable = brightyellow
set my_col_warning = color202
set my_col_shadow = color250
set my_col_shadow_bg = color234
EOF
else
    ## 8/16 colors.
    cat <<EOF
set my_col_builtin = brightblue
set my_col_comment = white
set my_col_constant = brightblue
set my_col_doc = green
set my_col_function = brightblue
set my_col_keyword = brightred
set my_col_preprocessor = magenta
set my_col_string = blue
set my_col_type = yellow
set my_col_variable = brightyellow
set my_col_warning = yellow
set my_col_shadow = brightwhite
set my_col_shadow_bg = brightblack
EOF
fi
