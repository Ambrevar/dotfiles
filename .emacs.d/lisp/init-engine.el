;;; Engine mode

(engine-mode)

;; TODO: Add (guix) ML search engines, tell them to open with eww.

(defengine arch-aur
  "https://aur.archlinux.org/packages.php?O=0&K=%s&do_Search=Go"
  :keybinding "aa")

(defengine arch-wiki
  "http://wiki.archlinux.org/index.php?title=Special%3ASearch&search=%s&go=Go"
  :keybinding "aw")

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
  :keybinding "e")

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

(defengine imdb
  "http://www.imdb.com/find?q=%s&s=all"
  :keybinding "i")

(defengine leo
  "http://dict.leo.org/frde/index_de.html#/search={}"
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
