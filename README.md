# Unix Home Configuration
## Author: P. Neidhardt
### 2013-10-18

Description
===========

This repository contains scripts and configuration files for various Unix
programs.  It targets Arch Linux and FreeBSD, but since these are "fundamental"
Unix distributions, it might work for most other Unices as well. I tried hard to
maintain universality: it should work anywhere (whatever the versions of the
programs are installed) with as few modifications as possible.

Most interesting parts include advanced configuration for Emacs, Mutt, and some
interesting shell scripts like a video transcoding wrapper and a smart audio
organizer using FFmpeg.

--------------------------------------------------------------------------------
Awesome
=======

* Extra deps: Vicious (Linux only)

Configuration is very close to the default one, which mostly fits my needs.
Most of the work I've done is for the status bar:  dynamic network speed (it
checks for different interfaces), CPU speed, dynamic battery that will not show
up if no battery is detected, and sound volume. But since I'm using the
Vicious plugin, it is quite straightforward.

This configuration is for now very version-sensitive, so you should make sure to
use latest build.

dwb
===

Some custom options, download location, search engines, plugins, etc.

Emacs
=====

* Extra deps
  * recommended: emacs-multiple-cursors emacs-xclip emacs-yasnippet
  * optional: emacs-bison-mode emacs-flex-mode emacs-glsl-mode emacs-lua-mode
    emacs-make-regexp emacs-mediawiki-bzr

Emacs daemon is integrated flawlessly thanks to a small script. See homeinit.
Because terminal and graphical clients do not behave the same way, it is
sometimes useful to specify which version should be used. For mutt, ranger
bulkrename, git commit message and dwb, it is required to wait for the client to
return.

Configuration for C programming. Bindings to compile either from makefile or
from a custom command if no makefile is found.

TeX and LaTeX full-featured home made environment: no AucTeX required, snippets,
dynamic compilation with various engines, dynamic configuration, proper PDF
view, temp files clean function, PDF compression, SyncTeX support.

Custom theme with 256 colors, works for both text and graphical Emacs.

Extended file support (shell files, Mutt mails, Arch Linux PKGBUILDs).

Some customization: columns, kill whole line, org-mode, semantic, etc.

Some custom functions: duplicate line, unfill-paragraph, etc.

A lot of code snippets, especially for LaTeX. Contains document template, plot
generation, pie chart generation, and others.

Mutt
====

* Compile options: IMAP and SMTP support.
* Patch: sidebar.
* Extra deps: antiword, ccrypt, fortune, sxiv, w3m.

This one is not easy to get into it. I managed to get multiple accounts with
encrypted passwords working, which means there is no plain password stored on
the disk. As a result, I just get prompted for a main password on Mutt startup,
then everything works out of the box.  I use ccrypt for password management
because it is much more simple than gnupg.

I'm using embedded IMAP and SMTP services which need appropriate compilation
options.

Some customization: custom index view, good Emacs integration (see .emacs),
custom theme (matches Emacs colors), various tweaks.

Thanks to the colorset.sh script, Mutt will check terminal color support and
load colors appropriately. Therefore this configuration should work on any
terminal, should it have 256-colors support or not.

URxvt has an URL support that works also for Mutt. For other terminals, you
might consider using the 'urlview' plugin for Mutt.

Ranger
======

* Extra deps: antiword, atool, highlight, img2txt, mediainfo, odt2txt, pdftotext,
unrtf, w3m.

Custom bindings, file association, tweaked a few options.

Scripts
=======

Probably the most interesting part here.

*clean: remove junk files from TeX projects, home folder, etc.

abs-wrapper: helper script to compile pacman-based packages (using
  makepkg). Using this you can recompile the whole system.

archive: simple tar wrapper to create archive for files and folders.

asciify: convert many non-ASCII characters to their nearest ASCII counterpart.

crun: quick way to execute C files.

ediff: diff with Emacs.

formatc: wrapper for 'indent' to prettify C source code.

homeinit: initialize a new home configuration, i.e. get needed files, create
  symlinks, etc.

netinit: network setup. Works with wpa_supplicant. May replace any network
  manager.

pacman-*: a lot of handy functions for the pacman package manager, like sorting
  by size with grand total, file listing with size, etc.

pdf*: PDF manipulation, e.g. extract pages, compress, resize to A4.

pkggen: generate lists of installed with pacman, FreeBSD's pkg and tlmgr (TeX
  Live manager).

tc-audio-*: smart audio transcoding implemented as shell functions. It will
  convert, tag and move the input tracks in a dynamic, yet customizable
  manner. It features a smart title case AWK function that will print almost any
  audio tag the proper way. It will copy cover files when found. You can preview
  the changes without writing data. Interesting parallelization with sed. Using
  FFmpeg.

tc-video-*: batch conversion of any kind of videos. Using FFmpeg.

translate: a tranlation frontend to Internet services. Default to stdin and
stdout (the Unix way), thus usable from your favorite text editor. 

Shell
=====

* Target: POSIX shell, dash, zsh

A lot of stuff: aliases, functions, shell options, etc.

Please note that this config is mainly intended -- and tested -- for zsh.  I
removed bash support since it is really a scourge to shell scripting. However
most of the scripts and the shell configuration are POSIX shell compatible
(tested with dash).

TeX
===

Plain TeX macros, most importantly a partial UTF-8 support (taken from LaTeX).

A lot of LaTeX macros. Among others: pie charts, dynamic plots with
input file and trend.

Plain TeX macros are in the '.texmf' folder. LaTeX macros are all bundled into
Emacs snippets so that .tex documents do not rely on any external file.

URxvt
=====

* Extra deps: Muennich's perl extensions (keyboard-select, clipboard, url-matcher)

Custom font and colors, no scroll bar, url-matcher, clipboard, and
keyboard-select.

--------------------------------------------------------------------------------
Usage
=====

The only purpose is to be a source of inspiration. Examples are always a good
technical support. (Especially for applications that do not provide examples in
their documentation.)  There is no use in blind-copying the content of any file
into your personal home folder. At best it might break things.

In case you still want to copy some files -- for quick and dirty testing purpose
-- do not forget that most of the files are in hidden folders.  Also note that
in some shells, the '*' joker will NOT match hidden files, that is

    cp -r source-dir/* dest-dir/

will copy non-hidden folders only.  To match all folders, use the following
joker instead:

    cp -r source-dir/{.*,*} dest-dir/   # zsh
    cp -r source-dir/{.??*,*} dest-dir/ # bash

Still, the solution for bash is not perfect as it affects 3 characters files
only. A more convenient solution:

    # bash only.
    shopt -s dotglob
    cp -r source-dir/* dest-dir/

Versioning
==========

Git makes it possible to use your home folder as a git repo, thus versioning
all files directly. To fetch source from Git repo:

    cd
    git init
    git remote add origin <repo>
    git fetch
    git branch master origin/master
    git checkout master

Some applications will need extra dependencies other than the default ones. You
might have a look at the list files in the .pkg/ folder to see what software
I've been using.

--------------------------------------------------------------------------------
Known issues
============

Emacs
-----

* When linum is on in very large files (5000+ lines), beginning-of-buffer is
  extremly slow when called from a shortcut, but not when called from the
  command mini-buffer.

* xclip mode will sometimes prevent yanking from working properly.

URxvt
-----

* Using the paste function from Muennich's clipboard on the same terminal where
  text was copied will make it hang (and crash).

* There is a bug with Xft anti-aliased font that prevent w3m image preview from
  working (too bad for ranger).

--------------------------------------------------------------------------------

Noteworthy apps
===============

* aalib
* abook
* aircrack-ng
* antiword
* apvlv
* asciidoc
* astyle
* atool
* awesome
* awk
* bashmount
* bc
* cabextract
* cal
* calc
* catdvi
* ccrypt
* cdrkit
* cdrtools
* centerim
* chrpath
* cmus
* column
* comm
* cppcheck
* cut
* dash
* dcraw
* diff
* dosbox
* doxygen
* driconf
* dtach
* dvtm
* dwb
* ecryptfs
* ecryptfs-simple
* emacs
* encfs
* fbpdf
* fbv
* fdisk
* feh
* ffmpeg
* file
* finch
* fmt
* fortune
* gaupol
* gcolor2
* gdb
* gimp
* gnuplot
* gparted
* graphicsmagick
* graphviz
* grep
* groff
* guile
* hdparm
* highlight
* htop
* id3v2
* imagemagick
* indent
* inkscape
* iotop
* irssi
* latex2html
* latex2rtf
* lrzip
* lsb-release
* lshw
* ltrace
* lua
* luakit
* mediainfo
* mkfs
* mkvtoolnix
* mplayer2
* mpv
* mutt
* nasm
* nawk
* ncdu
* nethogs
* newsbeuter
* ngrep
* nmap
* numlockx
* octave
* od
* odt2txt
* okular
* openshot
* openssh
* p7zip
* pari
* parted
* patch
* poppler
* pstotext
* pwgen
* qemu
* ranger
* re2c
* rsync
* rtorrent
* rxvt-unicode
* scrot
* sdlmame
* sdparm
* sed
* shred
* slock
* sort
* splint
* strace
* subdl
* submarine
* sudo
* surfraw
* sxiv
* sxlock
* syslinux
* task
* tcc
* tcpdump
* texi2html
* texinfo
* texlive
* textadept
* tig
* trash-cli
* tree
* udiskie
* unrtf
* unshield
* upx
* valgrind
* vim
* vlock
* vsftpd
* w3m
* weechat
* wipe
* wireshark
* wmfs
* wv
* x264
* xchm
* xclip
* xlockmore
* xosd
* yasm
* youtube-dl
* zathura
* zsh