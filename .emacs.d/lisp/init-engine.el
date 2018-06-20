;;; Engine mode

(engine-mode)
(require 'eww)

(defun ambrevar/engine-eww-function (url &optional _)
  (interactive)
  (eww url))

(defengine arch-aur
  "https://aur.archlinux.org/packages.php?O=0&K=%s&do_Search=Go"
  :keybinding "aa")

(defengine arch-packages
  "https://www.archlinux.org/packages/?sort=&q=%s&maintainer=&flagged="
  :keybinding "ap")

(defengine arch-wiki
  "http://wiki.archlinux.org/index.php?title=Special%%3ASearch&search=%s&go=Go"
  :keybinding "aw"
  :browser 'ambrevar/engine-eww-function)

(defengine ctan
  "http://www.ctan.org/search?phrase=%s"
  :keybinding "c")

(defengine devdocs
  "http://devdocs.io/#q=%s"
  :keybinding "dd")

(defengine duckduckgo
  "https://duckduckgo.com/?q=%s"
  :keybinding "dg")

(defengine emacs-debbugs
  "https://debbugs.gnu.org/cgi/pkgreport.cgi?package=emacs;include=subject%3A%s;repeatmerged=on;archive=both"
  :keybinding "eb"
  :browser 'ambrevar/engine-eww-function)

(defengine emacs-devel
  "https://lists.gnu.org/archive/cgi-bin/namazu.cgi?idxname=emacs-devel&submit=Search&query=%s"
  :keybinding "ed"
  :browser 'ambrevar/engine-eww-function)

(defengine emms-help
  "https://lists.gnu.org/archive/cgi-bin/namazu.cgi?idxname=emms-help&submit=Search!&query=%s"
  :keybinding "ee"
  :browser 'ambrevar/engine-eww-function)

(defengine gentoo
  "https://wiki.gentoo.org/index.php?title=Special%3ASearch&search=%s&go=Go"
  :keybinding "ge")

(defengine github
  "https://github.com/search?ref=simplesearch&q=%s"
  :keybinding "gh")

(defengine goodreads
  "https://www.goodreads.com/search?q=%s"
  :keybinding "gr")

(defengine google-maps
  "https://maps.google.com/maps?q=%s"
  :keybinding "gm")

(defengine guix-devel
  "https://lists.gnu.org/archive/cgi-bin/namazu.cgi?idxname=guix-devel&submit=Search&query=%s"
  :keybinding "gud"
  :browser 'ambrevar/engine-eww-function)

(defengine guix-help
  "https://lists.gnu.org/archive/cgi-bin/namazu.cgi?idxname=help-guix&submit=Search!&query=%s"
  :keybinding "guh"
  :browser 'ambrevar/engine-eww-function)

(defengine imdb
  "http://www.imdb.com/find?q=%s&s=all"
  :keybinding "i")

(defengine musicbrainz
  "http://musicbrainz.org/search?query=%s&type=artist&method=indexed"
  :keybinding "mb")

(defengine openstreetmap
  "https://www.openstreetmap.org/search?query=%s"
  :keybinding "osm")

(defengine stackoverflow
  "http://stackoverflow.com/search?q=%s"
  :keybinding "s")

(defengine torrent-kickass
  "https://kickass.unblocked.live/search.php?q=%s"
  :keybinding "tk")

(defengine torrent-piratebay
  "https://ukpirate.click/s/?q=%s"
  :keybinding "tp")

(defengine wikipedia
  "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
  :keybinding "wp"
  :docstring "Search Wikipedia!")

(defengine wikibooks
  "http://en.wikibooks.org/wiki/Special:Search?search=%s"
  :keybinding "wb")

(defengine wiktionary
  "http://en.wiktionary.org/wiki/Special:Search?search=%s"
  :keybinding "wk")

(defengine wine-appdb
  "http://www.winehq.org/search/?q=%s"
  :keybinding "wk")

(defengine youtube
  "http://www.youtube.com/results?search_query=%s"
  :keybinding "y")

(load "~/personal/bookmarks/engines.el" t)

(provide 'init-engine)
