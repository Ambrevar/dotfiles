# Unix Home Configuration
### Author: P. Neidhardt

## Overview

This repository contains scripts and configuration files for various Unix
programs. I tried hard to maintain universality: it should work anywhere (Arch
Linux, FreeBSD...), with whichever version of the programs. In the worst case,
very few modifications will be necessary.

Most interesting parts include advanced configuration for Awesome, Emacs, Mutt,
ranger, Zsh and some shell scripts.

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

### dwb

Some custom options, download location, search engines, plugins, etc.
Custom "fast-forward" javascript.

### Emacs

Emacs daemon is flawlessly integrated thanks to a small _em_ script -- see the
scripts folder. Unlike the terminal client, the graphical client does not return
and spawns a new window. For these reasons the terminal client can prove useful
in some contexts: Mutt, ranger bulkrename, git and dwb. I wrote a lot of Lisp
helper functions and a custom theme with 256 colors, works for both text and
graphical Emacs.

C-based modes: bindings to compile either from makefile or from a custom command
if no makefile is found, formatting with Uncrustify, skeletons.

TeX and LaTeX full-featured environment: no AucTeX, itemize function, skeletons,
compilation with various engines and index support, external PDF view with
SyncTeX support, temp files clean function, PDF compression.

### Mutt

* Compile options: IMAP and SMTP support.
* Patch: sidebar.
* Extra deps: antiword, ccrypt, fortune, sxiv, w3m.

Support for multiple accounts with encrypted passwords. (Mutt will prompt for
the master password only once.) I use ccrypt for password management because it
is much simpler than gnupg.

I use embedded IMAP and SMTP services which need appropriate compilation
options. Embedded IMAP does not need to store e-mails locally. To speed-up
e-mail queries, I cache them on disk. When the disk usage limit is reached, the
older cached e-mails are erased until the cache shrink below the limit again.

Some customization: custom index view, good Emacs integration (see .emacs.d),
custom theme (matches Emacs colors), various tweaks.

For universal terminal support, the colorset.sh script will check terminal
capabilities and load colors appropriately.

URxvt has an URL support that works also for Mutt. For other terminals, you
might consider using the 'urlview' plugin for Mutt.

### Ranger

* Extra deps: antiword, atool, highlight, img2txt, mediainfo, odt2txt, pdftotext,
unrtf, w3m.

Custom bindings, file association, tweaked a few options, additional functions
(mkcd, extracthere...).

### Scripts

Probably the most interesting part here.

* archive: simple tar wrapper to create archive for files and folders.

* asciify: convert many non-ASCII characters to their nearest ASCII counterpart.

* bsdman: read *BSD man pages.

* crun: quick way to execute C files.

* currency: currency converter.

* dataindex: create index of hierarchies. Useful to keep track of folder content
  and structure.

* ediff: diff with Emacs.

* einfo: info viewer with Emacs.

* elisp: Emacs Lisp interpreter using Emacs.

* extract: simple tar wrapper to extract archives. Atool should be preferred if
  available.

* git-*: some git helper functions for sync and so on.

* homeinit: initialize a new home configuration, i.e. get needed files, create
  symlinks, etc.

* imagemount: a CDEmu/fuseiso wrapper that creates/deletes virtual drives
  automatically.

* mover: move and merge folder into destination.

* netinit: network setup. Works with wpa_supplicant. May replace any network
  manager.

* pac*: pacman helper functions.

* pdf*: PDF manipulation, e.g. extract pages, compress, resize to A4.

* pkggen: generate lists of installed with pacman, FreeBSD's pkg and tlmgr (TeX
  Live manager).

* tc-video-*: batch conversion of any kind of videos. Using FFmpeg.

### Shell

* Target: POSIX shell, dash, zsh

Features: aliases, functions, shell options, etc.
Most of the shell configuration is POSIX and works with dash.

### TeX

Plain TeX macros, most importantly a partial UTF-8 support (taken from LaTeX).
Plain TeX macros are in the '.texmf' folder. Some Asymptote functions.

### URxvt

* Extra deps: Muennich's perl extensions (keyboard-select, clipboard, url-matcher)

Custom font and colors, no scroll bar, url-matcher, clipboard, and
keyboard-select.

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
might have a look at the lists in the `.pkg/` folder to know what software I
have ve been using.

--------------------------------------------------------------------------------
## Known issues

* All scripts take a strong point at being POSIX. However, there is no POSIX way
  (at least to my knowledge) to execute a shell function on the result of a
  'find'. Neither 'find' nor 'xargs' can execute shell functions. One solution
  to this would be to call an external script, but then we lose all global
  variables.  For now we assume no input file has newline, which is quite a
  strong assumption and not safe at all.
