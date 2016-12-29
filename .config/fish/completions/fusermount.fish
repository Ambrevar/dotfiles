## TODO: Remove this fix when upstream has merged it.
## https://github.com/fish-shell/fish-shell/pull/3642
complete -c fusermount --description "Mount point" -x -a '(__fish_print_mounted)'
complete -c fusermount -s h --description "Display help and exit"
complete -c fusermount -s v --description "Display version and exit"
complete -c fusermount -s o -x --description "Mount options"
complete -c fusermount -s u --description "Unmount"
complete -c fusermount -s q --description "Quiet"
complete -c fusermount -s z --description "Lazy unmount"
