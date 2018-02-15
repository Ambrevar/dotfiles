;;; MediaWiki

;;; This mode has numerous issue with network queries:
;;; https://github.com/hexmode/mediawiki-el/issues/
;;; Besides you can try extending `url-cookie-save-interval'.
;;; Nonetheless is mode is helpfull for syntax awareness.

(ambrevar/define-keys mediawiki-mode-map
                      "C-c o" 'mediawiki-browse
                      "M-g" nil ; This shadows M-g M-g.
                      "C-x C-s" 'save-buffer
                      "C-c M-s" 'mediawiki-save)

(setq
 mediawiki-site-alist
 '(("Wikipedia" "http://en.wikipedia.org/w/" "Ambrevar" "" "Main Page")
   ("Wikibooks" "http://en.wikibooks.org/w/" "Ambrevar" "" "LaTeX")
   ("ArchLinux" "https://wiki.archlinux.org/" "Ambrevar" "" "Main Page")
   ("WikEmacs" "https://wikemacs.org/wiki/" "Ambrevar" "" "Main Page")))

(add-hook 'mediawiki-mode-hook 'visual-line-mode)
(add-hook 'mediawiki-mode-hook 'turn-off-auto-fill)

(define-skeleton ambrevar/mediawiki-tt
  "Insert truetype text."
  nil "<tt>" @ _ "</tt>" @)

(provide 'init-mediawiki)
