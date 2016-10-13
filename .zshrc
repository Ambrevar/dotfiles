## Zsh master file

loadrc () {
	for i; do
		[ -f "${SHELL_DIR}/$i" ] && . "${SHELL_DIR}/$i"
	done
}

## Options should be sourced first, before the SHELL_FILEBROWSER hook.
loadrc options_zsh
loadrc keys_zsh
loadrc alias_rc colors_rc completion_rc funs_rc

## Assign a binding
##   sh -c 'export SHELL_FILEBROWSER=true; exec $TERMCMD'
## and this will autostart the file browser.
[ -n "$SHELL_FILEBROWSER" ] && unset SHELL_FILEBROWSER && browse

## Start at a specific location. Useful when switching to a shell from a browser
## for instance.
[ -n "$SHELL_CD" ] && cd "$SHELL_CD" && unsert SHELL_CD

## Shell hook: should be sourced last.
loadrc hook
