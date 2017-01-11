# If not running interactively, don't do anything
[[ $- != *i* ]] && return

## Shell-independant code: everything we set here must be exported before we run
## our favorite shell.

## Enable color support of ls.
if [ "$TERM" != "dumb" ]; then
	if [ "$OSTYPE" = "linux-gnu" ]; then
		eval "$(dircolors "$HOME/.dircolorsdb")"
	else
		export LSCOLORS="Ex"
		# export LSCOLORS="ExfxcxDxCxdxdxCxCxECEh"
	fi
fi

for shell in fish zsh; do
	command -v $shell >/dev/null 2>&1 && exec $shell
done
