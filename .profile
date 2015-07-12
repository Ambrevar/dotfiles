#!/bin/sh
################################################################################
## .profile
################################################################################
## This file is sourced by .xprofile and shell rc files to make sure it work in
## TTY as well as under X.

## Mask
## Result for 027 is: rwxr-x---
umask 027

## Note that it is important for MANPATH to have an empty entry to keep
## searching in the default db path. Same for INFOPATH, which should have an
## empty entry at the end, otherwise Emacs will not use standard locations. For
## security (and bad programming assumptions) you should always append entries
## to PATH, not prepend them.
appendpath() {
	[ $# -eq 2 ] && PATHVAR=$2 || PATHVAR=PATH
	if [ -z "$(eval echo \$$PATHVAR | grep "\(:\|^\)$1\(:\|$\)")" ]; then
		eval export $PATHVAR="\$$PATHVAR:$1"
	fi
}
prependpath() {
	[ $# -eq 2 ] && PATHVAR=$2 || PATHVAR=PATH
	if [ -z "$(eval echo \$$PATHVAR | grep "\(:\|^\)$1\(:\|$\)")" ]; then
		eval export $PATHVAR="$1:\$$PATHVAR"
	fi
}

appendpath "${HOME}/.launchers/"
appendpath "${HOME}/.scripts/"
prependpath "${HOME}/.hackpool/"
[ -d /usr/lib/surfraw ] && appendpath "/usr/lib/surfraw"

## TeXlive
TEXDIR="${TEXDIR:-/usr/local/texlive}"
if [ -d "${TEXDIR}" ]; then
	TEXYEAR=$(/bin/ls -1r "${TEXDIR}" | grep -m1 "[0-9]\{4\}")
	TEXDISTRO=$(uname -m)-$(uname | tr "[[:upper:]]" "[[:lower:]]")
	TEXFOLDER="${TEXDIR}/${TEXYEAR}/bin/${TEXDISTRO}/"
	if [ -d "${TEXFOLDER}" ]; then
		appendpath $TEXFOLDER
		prependpath ${TEXDIR}/${TEXYEAR}/texmf/doc/info INFOPATH

		## BSD uses 'manpath' utility, so MANPATH variable may be empty.
		if [ "$OSTYPE" = "linux-gnu" ]; then
			prependpath ${TEXDIR}/${TEXYEAR}/texmf/doc/man MANPATH
		fi
	fi
	unset TEXYEAR
	unset TEXDISTRO
	unset TEXFOLDER
fi
unset TEXDIR
export BIBINPUTS=~/dataperso/bibliography

## Plan9
PLAN9DIR="/opt/plan9"
if [ -d "$PLAN9DIR" ]; then
	appendpath "$PLAN9DIR/bin"
	if [ "$OSTYPE" = "linux-gnu" ]; then
		appendpath "$PLAN9DIR/share/man" MANPATH
	fi
fi
unset PLAN9DIR

## Less config. -R is needed for lesspipe.
export LESS=' -R '
## Make 'less' more friendly for non-text input files, see lesspipe(1).
command -v lesspipe >/dev/null 2>&1 && eval "$(lesspipe)"
## Remove less history.
LESSHISTFILE='-'

## Manpage.
export MANPAGER="less -s"
export MANWIDTH=80
## The following options are useful for FreeBSD default 'less' command which has
## an empty prompt. Sadly this gets messy with 'apropos'.
# export MANPAGER="less -sP '?f%f .?m(file %i of %m) .?ltlines %lt-%lb?L/%L. .byte %bB?s/%s. ?e(END) :?pB%pB\%..%t'"


## Time display (with ls command for example)
## TODO: BSD version?
export TIME_STYLE=+"|%Y-%m-%d %H:%M:%S|"

## Default text editor
EDITOR="nano"
command -v vim >/dev/null 2>&1 && EDITOR="vim"
command -v emacs >/dev/null 2>&1 && EDITOR="emacs"
GIT_EDITOR="$EDITOR"

## 'em' is a script for emacsclient. See '.scripts/em'.
if command -v em >/dev/null 2>&1; then
	EDITOR='em'
	GIT_EDITOR='emc'
fi

export EDITOR
export GIT_EDITOR

## Internet Browser
for i in dwb luakit google-chrome; do
	command -v $i >/dev/null 2>&1 && export BROWSER=$i && break
done

## SSH-Agent
## WARNING: this is insecure on machines where someone else has root access.
command -v ssh-agent >/dev/null 2>&1 && eval "$(ssh-agent)"
## Kill ssh-agent on session end. Console login only.
command -v sessionclean >/dev/null 2>&1 && trap 'sessionclean' 0

## Set TEMP dir if you want to override /tmp for some applications that check
## for this variable. Usually not a good idea since some applications will write
## junk files in it.
# [ -d "$HOME/temp" ] && export TEMP="$HOME/temp"

## Wine DLL override. This removes the annoying messages for Mono and Gecko.
export WINEDLLOVERRIDES="mscoree,mshtml="

## Go
if [ -d "$HOME/.go" ]; then
	export GOPATH=~/.go
	appendpath "$GOPATH/bin"
fi

## Startup error log.
## dmesg
log_dmesg="$(dmesg | grep -i error)"
[ -n "$log_dmesg" ] && echo "$log_dmesg" > "$HOME/errors-dmesg.log" || rm -f "$HOME/errors-dmesg.log"
## systemd
if command -v systemctl >/dev/null 2>&1; then
	count="$(systemctl -l --failed | awk '{print $1;exit}')"
	if [ $count -ne 0 ]; then
		systemctl -l --failed > "$HOME/errors-systemd.log"
	else
		rm -f "$HOME/errors-systemd.log"
	fi
fi

## Hook. Should be sourced last
[ -f ~/.profile_hook ] && . ~/.profile_hook
################################################################################
## Hook example
#
# appendpath "${HOME}/local/usr/bin"
# prependpath "${HOME}/local/usr/share/info" INFOPATH
# prependpath "${HOME}/local/usr/share/man" MANPATH
#
# appendpath "$HOME/local/usr/include" C_INCLUDE_PATH
# appendpath "$HOME/local/usr/include" CPLUS_INCLUDE_PATH
# appendpath "$HOME/local/usr/lib" LIBRARY_PATH
# export CPPFLAGS=-I$HOME/local/usr/include
# export LDFLAGS=-L$HOME/local/usr/lib
#
# appendpath "${HOME}/local/usr/lib/pkgconfig" PKG_CONFIG_PATH
#
# appendpath "${HOME}/local/usr/lib/" LD_LIBRARY_PATH
# appendpath "$HOME/local/usr/lib/python2.7/dist-packages/" PYTHONPATH
# export LUA_CPATH="$HOME/local/usr/lib/lib?.so;$(lua -e "print(package.cpath)")"
#
# umask 077
################################################################################
