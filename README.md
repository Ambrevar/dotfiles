# Unix Home Configuration
### Author: P. Neidhardt

## Overview

This repository contains scripts and configuration files for various Unix
programs. I tried hard to maintain universality: it should work anywhere (Arch
Linux, FreeBSD...), with whichever version of the programs. In the worst case,
only few modifications will be necessary.

Most interesting parts include advanced configuration for Awesome, Emacs, fish,
Mutt, ranger, and some scripts.

--------------------------------------------------------------------------------
## Description

### Awesome

* Extra deps: Vicious (Linux only)

Configuration is very close to the default one with some features removed
(bindings, mouse control, context menu). The main new feature is the status bar:
dynamic network speed (it checks for different interfaces), CPU speed, a battery
indicator that will not show up if no battery is detected, and sound volume. The
implementation is straightforward since I am using the Vicious plugin.

This configuration is very version-sensitive, so make sure to use the latest
build.

### Emacs

See my [Emacs pro-tips](http://ambrevar.bitbucket.org/emacs/).

### fish & fzf

fish's default configuration satisfies most of my need. I make intense use of
`fzf` with custom functions and bindings, such as `fzf-select` and
`fzf-complete`.

A few wrappers:

- man: color and justify to window width.

- cd: keeps a bigger history and suppress duplicates. fish starts at last
visited location.

I also have a `fzf-cdhist-widget` to go back to any location in just blink.

fzf can display a preview. I use a `preview` wrapper script over ranger's
`scope.sh`. Extra deps: antiword, atool, highlight, img2txt, mediainfo, odt2txt,
pdftotext, unrtf, w3m.

### Mutt

* Compile options: IMAP and SMTP support, sidebar.
* Extra deps: antiword, ccrypt, fortune, sxiv, w3m.

Support for multiple accounts with encrypted passwords. (Mutt will prompt for
the master password only once.) I use ccrypt for password management because it
is much simpler than gnupg.

I use embedded IMAP and SMTP services which need appropriate compilation
options. Embedded IMAP does not need to store e-mails locally. To speed-up
e-mail queries, I cache them on disk. When the disk usage limit is reached, the
older cached e-mails are erased until the cache shrinks below the limit again.

Some customization: custom index view, good Emacs integration (see `.emacs.d`),
custom theme (matches Emacs colors), various tweaks.

For universal terminal support, the `colorset.sh` script will check terminal
capabilities and load colors appropriately.

URxvt has an URL support that works also from Mutt. For other terminals, you
might consider using the `urlview` plugin for Mutt.

### Scripts

Probably the most interesting part here.

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

--------------------------------------------------------------------------------
## Versioning

Git makes it possible to use your home folder as a git repo, thus versioning
all files directly.

	cd
	git init
	git remote add origin <repo>
	git fetch
	git branch master origin/master
	git checkout master

Some applications will need extra dependencies other than the default ones. You
might have a look at the lists in the `.pkg/` folder to know what programs I am
using.
