# .dotfiles
### Author: P. Neidhardt

## Overview

I use of Emacs for almost everything, including my window manager (EXWM).  See
my [Emacs pro-tips](http://ambrevar.bitbucket.io/emacs/).

As such, the more-or-less advanced configurations of my former favourite Unix
programs are gone (Awesome, cmus, fish, fzf, Mutt, newsbeuter, ranger, urxvt,
zathura, zsh).  You can search for them before the git commit `README: The Big
Emacs Shift`.



I've also written a fair amount of scripts, most of which are POSIX.  The highlights include:

* asciify: convert many non-ASCII characters to their nearest ASCII counterpart.

* bsdman: read *BSD man pages.

* crun: quick way to execute C files.

* dataindex: create index of hierarchies. Useful to keep track of folder content
and structure.

* ediff: diff with Emacs.

* einfo: info viewer with Emacs.

* elisp: Emacs Lisp interpreter using Emacs.

* git-*: some git helper functions for sync and so on.

* homeinit: initialize a new home configuration, i.e. get needed files, create
symlinks, etc.

* imagemount: a CDEmu/fuseiso wrapper that creates/deletes virtual drives
automatically.

* mover: move and merge folder into destination.

* pac*: pacman helper functions.

* pdfctl: PDF manipulation, e.g. extract pages, compress, resize to A4.

* pkglister: generate lists of installed with pacman, FreeBSD's pkg and tlmgr
(TeX Live manager).

* tc-video-*: batch conversion of any kind of videos. Using FFmpeg.

## Versioning

Git makes it possible to use your home folder as a git repo, thus versioning
all files directly.

	cd
	git init
	git remote add origin <repo>
	git fetch
	git checkout master

For the list of programs I currently use, see the `.pkglists/` folder.
