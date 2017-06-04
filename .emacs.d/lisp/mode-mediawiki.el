;; MediaWiki

;; This mode has numerous issue with network queries:
;; https://github.com/hexmode/mediawiki-el/issues/
;; Besides you can try extending `url-cookie-save-interval'.
;; Nonetheless is mode is helpfull for syntax awareness.

(local-set-key (kbd "C-c o") 'mediawiki-browse)
(local-unset-key (kbd "M-g")) ; This shadows M-g M-g.
(local-set-key (kbd "C-x C-s") 'save-buffer)
(local-set-key (kbd "C-c M-s") 'mediawiki-save)

(setq
 mediawiki-site-alist
 '(("Wikipedia" "http://en.wikipedia.org/w/" "Ambrevar" "" "Main Page")
   ("Wikibooks" "http://en.wikibooks.org/w/" "Ambrevar" "" "LaTeX")
   ("ArchLinux" "https://wiki.archlinux.org/" "Ambrevar" "" "Mutt")
   ("WikEmacs" "https://wikemacs.org/wiki/" "Ambrevar" "" "Main Page")))

(add-hook-and-eval 'mediawiki-mode-hook 'visual-line-mode)
(add-hook-and-eval 'mediawiki-mode-hook 'turn-off-auto-fill)

(define-skeleton mediawiki-tt
  "Insert truetype text."
  nil "<tt>" @ _ "</tt>" @)

(provide 'mode-mediawiki)
