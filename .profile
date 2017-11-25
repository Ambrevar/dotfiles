#!/bin/sh
## This file should be automatically sourced by the login manager or Bash if
## .bash_profile does not exist.  If this file is not automatically sourced,
## do it from the shell config to me sure it applies to TTY as well.

## Mask
## Result for 027 is "rwxr-x---".  022 is the popular default.
##
## As a result applications make the bad assumption # that "others" have access.
## Another drawback of 027 is that is behaves badly with default sudo config: for
## instance "sudo mkdir foo" will effectively create a "foo" folder whose owner
## is root and with permission 027, even if root's umask is 022.  This is
## usually very bad.
## See https://wiki.archlinux.org/index.php/Sudo#Permissive_umask.
##
## It is possible to override sudo's umask by adding the following to the
## sudoers file:
##
## Defaults umask = 0022
## Defaults umask_override
# umask 027



## Preliminary path definitions.  For security reasons (and bad programming
## assumptions) you should always append entries to PATH, not prepend them.
appendpath () {
	[ $# -eq 2 ] && PATHVAR=$2 || PATHVAR=PATH
	[ -d "$1" ] || return
	eval echo \$$PATHVAR | grep -q "\(:\|^\)$1\(:\|$\)" && return
	eval export $PATHVAR="\$$PATHVAR:$1"
}
prependpath () {
	[ $# -eq 2 ] && PATHVAR=$2 || PATHVAR=PATH
	[ -d "$1" ] || return
	eval echo \$$PATHVAR | grep -q "\(:\|^\)$1\(:\|$\)" && return
	eval export $PATHVAR="$1:\$$PATHVAR"
}

## Use this to override system executables.
prependpath "${HOME}/personal/hackpool"

## TeXlive
TEXDIR="${TEXDIR:-/usr/local/texlive}"
if [ -d "${TEXDIR}" ]; then
	TEXYEAR=$(/bin/ls -1r "${TEXDIR}" | grep -m1 "[0-9]\{4\}")
	TEXDISTRO=$(uname -m)-$(uname | awk '{print tolower($0)}')
	TEXFOLDER="${TEXDIR}/${TEXYEAR}/bin/${TEXDISTRO}"
	if [ -d "${TEXFOLDER}" ]; then
		appendpath $TEXFOLDER
		## Same for INFOPATH, which should have an empty entry
		## at the end, otherwise Emacs will not use standard locations.
		prependpath ${TEXDIR}/${TEXYEAR}/texmf/doc/info INFOPATH

		if [ "$(uname -o)" = "GNU/Linux" ]; then
			## Under GNU/Linux, MANPATH must contain one empty entry for 'man' to
			## lookup the default database.  Since BSD uses 'manpath' utility, the
			## MANPATH variable is not needed.
			prependpath ${TEXDIR}/${TEXYEAR}/texmf/doc/man MANPATH
		fi
	fi
	unset TEXYEAR
	unset TEXDISTRO
	unset TEXFOLDER
fi
unset TEXDIR
export BIBINPUTS=~/personal/bibliography

## Plan9 (base)
PLAN9DIR="/opt/plan9"
if [ -d "$PLAN9DIR" ]; then
	## No need to add to path if /etc/profile.d/plan9.sh does it already.
	# appendpath "$PLAN9DIR/bin"
	if [ "$(uname -o)" = "GNU/Linux" ]; then
		appendpath "$PLAN9DIR/share/man" MANPATH
	fi
fi
unset PLAN9DIR

## Go
if [ -d "$HOME/go" ]; then
	export GOPATH=~/go:~/.go-tools
	appendpath "$HOME/.go-tools/bin"
	appendpath "$HOME/go/bin"
	command -v godoc >/dev/null 2>&1 && godoc -http :6060 -play 2>/dev/null &
fi

## Last PATH entries.
appendpath "/usr/lib/surfraw"
appendpath "${HOME}/personal/games/launchers"
appendpath "${HOME}/.local/bin"
command -v pacman >/dev/null 2>&1 && appendpath "${HOME}/.local/bin/pacman"



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

## SSH-Agent
if command -v ssh-agent >/dev/null 2>&1 && [ -z "$SSH_AGENT_PID" ]; then
	 eval "$(ssh-agent)"
	 ## Kill ssh-agent on session end. Console login only.
	 trap 'test -n "$SSH_AGENT_PID" && eval $(ssh-agent -k)' 0
fi

## Linux specific
if [ "$(uname -o)" = "GNU/Linux" ] ; then
	## Startup error log.
	## dmesg
	log_dmesg="$(dmesg | grep -i error)"
	[ -n "$log_dmesg" ] && echo "$log_dmesg" > "$HOME/errors-dmesg.log" || rm "$HOME/errors-dmesg.log" 2>/dev/null
	## systemd
	if command -v systemctl >/dev/null 2>&1; then
		count="$(systemctl show | awk -F= '$1=="NFailedUnits" {print $2; exit}')"
		if [ $count -ne 0 ]; then
			systemctl -l --failed > "$HOME/errors-systemd.log"
		else
			rm -f "$HOME/errors-systemd.log"
		fi
	fi

	## Set sound volume.
	amixer 2>/dev/null | grep -q PCM && amixer set PCM 100%

	## External device auto-mounting.
	## If already started, the new process will replace the old one.
	if command -v udiskie >/dev/null 2>&1; then
		udiskie &
	fi
fi

## Wine DLL overrides.
## Remove the annoying messages for Mono and Gecko.
export WINEDLLOVERRIDES="mscoree,mshtml="
## Do not create desktop links or start menu entries.
export WINEDLLOVERRIDES="$WINEDLLOVERRIDES;winemenubuilder.exe=d"

## Pacman asp root.
if command -v asp >/dev/null 2>&1; then
	export ASPROOT="$HOME/.cache/asp"
fi

## Default text editor
## 'em' is a custom wrapper for emacsclient. See '.bin/em'.
## VISUAL is given priority by some programs like Mutt. This way we can separate
## editors that wait from those that don't.
for i in emacsclient em emacs vim vi nano; do
	command -v $i >/dev/null 2>&1 && export EDITOR=$i && break
done
GIT_EDITOR="$EDITOR"
VISUAL="$EDITOR"
[ "$GIT_EDITOR" = em ] && GIT_EDITOR=emc
[ "$VISUAL" = em ] && VISUAL=emw
export GIT_EDITOR
export VISUAL

## $HOME software install
## See http://nullprogram.com/blog/2017/06/19/.
## The variables should not contain paths to non-existing folders as it may
## break compilers.
prependpath "$HOME/.local/include" C_INCLUDE_PATH
prependpath "$HOME/.local/include" CPLUS_INCLUDE_PATH
prependpath "$HOME/.local/lib" LIBRARY_PATH
prependpath "$HOME/.local/lib/pkgconfig" PKG_CONFIG_PATH
prependpath "$HOME/.local/share/info" INFOPATH
prependpath "$HOME/.local/share/man" MANPATH
## If you install a library in your home directory that is also installed on the
## system, and then run a system program, it may be linked against your library
## rather than the library installed on the system as was originally
## intended. This could have detrimental effects.
# export LD_LIBRARY_PATH=$HOME/.local/lib

## Hook. Should be sourced last
[ -f ~/.profile_hook ] && . ~/.profile_hook
## Hook example
#
# export CPPFLAGS=-I$HOME/local/usr/include
# export LDFLAGS=-L$HOME/local/usr/lib
#
# appendpath "$HOME/local/usr/lib/python2.7/dist-packages/" PYTHONPATH
# export LUA_CPATH="$HOME/local/usr/lib/lib?.so;$(lua -e "print(package.cpath)")"
#
# umask 077

## End: Source .bashrc. The rc file should guard against non-interactive shells.
[ "$(ps -o comm= $$)" != bash ] && return
[ -f ~/.bashrc ] && . ~/.bashrc

[ -z "$DISPLAY" ] && [ "$(tty)" = '/dev/tty1' ] && exec xinit -- vt01
