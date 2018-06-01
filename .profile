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

## TeXlive -- TODO: Remove once guix has packaged it all properly.
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

## Go
if [ -d "$HOME/go" ]; then
	export GOPATH=~/go:~/.go-tools
	appendpath "$HOME/.go-tools/bin"
	appendpath "$HOME/go/bin"
	command -v godoc >/dev/null 2>&1 && godoc -http :6060 -play 2>/dev/null &
fi

## Cask
appendpath "$HOME/.cask/bin"

## Last PATH entries.
appendpath "/usr/lib/surfraw"
appendpath "${HOME}/personal/games/launchers"
appendpath "${HOME}/.local/bin"
command -v pacman >/dev/null 2>&1 && appendpath "${HOME}/.local/bin/pacman"

## mcron: needs to be run after PATH is fully set or else local programs could
## be missing.
if command -v mcron >/dev/null 2>&1; then
	 mcron &
fi

## Remove less history.
LESSHISTFILE='-'

## Manpage.
export MANPAGER="less -s"
export MANWIDTH=80

## Time display (with ls command for example).  GNU 'ls' only.
export TIME_STYLE=+"|%Y-%m-%d %H:%M:%S|"

## SSH-Agent
## Set SSH to use gpg-agent
unset SSH_AGENT_PID
if [ "${gnupg_SSH_AUTH_SOCK_by:-0}" -ne $$ ]; then
  export SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)"
fi
# Set GPG TTY
export GPG_TTY=$(tty)
# Refresh gpg-agent tty in case user switches into an X session
gpg-connect-agent updatestartuptty /bye >/dev/null

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

	## Set sound volume.  (Useless when running Pulseaudio.)
	# amixer 2>/dev/null | grep -q PCM && amixer set PCM 100%

	## External device auto-mounting.
	## If already started, the new process will replace the old one.
	if command -v udiskie >/dev/null 2>&1; then
		udiskie &
	elif command -v devmon >/dev/null 2>&1; then
		devmon &
	else
		udisks-automount &
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

## Guix
if command -v guix >/dev/null 2>&1; then
	export GUIX_PACKAGE_PATH="$HOME/.guix-packages"
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
