# .dotfiles

## Overview

I use of Emacs for almost everything, including my window manager (EXWM).  See
my [Emacs pro-tips](http://ambrevar.bitbucket.io/emacs/).

As such, the more-or-less advanced configurations of my former favourite Unix
programs are gone (Awesome, cmus, fish, fzf, Mutt, newsbeuter, ranger, urxvt,
zathura, zsh).  You can search for them before the git commit `README: The Big
Emacs Shift`.

## Setup

For the list of programs I currently use, see the `.pkglists/` folder.

The `homeinit` script fully bootstraps a user profile with required files,
folders, symlinks applications.

The `homeclean` script removes trash files, caches and warns if critically
private data is found (e.g. PGP keys).

The `homesync` script updates the package lists, prints the status of all known
projects and optionally pushes the unmerged changes upstream.

As for managing a dotfiles repository, there are various approaches.


### Direct versioning

Git makes it possible to use your home folder as a git repo, thus versioning
all files directly.

	cd
	git init
	git remote add origin <repo>
	git fetch
	git checkout master

### GNU Stow

[GNU Stow](https://www.gnu.org/software/stow/) lets you symlink a project's
files to an arbitrary folder.

The simplest setup would be to clone the dotfiles to, say, `~/dotfiles` then run

	cd ~/dotfiles
	stow .

This has several advantages over direct versioning:

- Subfolders in home are not subject to being included into the dotfiles git
repository.  This is especially relevant for projects under a version control
system other than git.

- No need for a `.gitignore`.

- Simplified file control (add/remove/etc.).

- You can fine-tune which program configuration to synchronize on a per-system basis.

- You can manage several configurations for the same programs.
