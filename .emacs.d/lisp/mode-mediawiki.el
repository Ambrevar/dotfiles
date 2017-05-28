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

;; Skeletons

(define-skeleton mediawiki-latex-link
  "Insert link to the LaTeX wikibook."
  "Link: " "[[LaTeX/" @ str "|" str "]]" _)

(define-skeleton mediawiki-latex-env
  "Insert LaTeX environment."
  nil "{{LaTeX/Environment|" @ _ "}}")

(define-skeleton mediawiki-latex-example
  "Insert LaTeX example."
  "Code: "
  \n "{{LaTeX/Example|code=" @ str \n
  "|render=" \n
  _ \n
  "}}" \n)

(define-skeleton mediawiki-latex-param
  "Insert LaTeX parameter."
  nil "{{LaTeX/Parameter|" @ _ "}}")

(define-skeleton mediawiki-latex-package
  "Insert LaTeX package."
  "{{LaTeX/Package|" @ _ "}}")

(define-skeleton mediawiki-latex-usage
  "Insert LaTeX usage block."
  nil "{{LaTeX/Usage|code=" \n
  @ _ \n
    "}}")

(define-skeleton mediawiki-latex-inline
  "Insert LaTeX inline code."
  nil "{{LaTeX/LaTeX|code=" @ _ "<!---->}}")


(define-skeleton mediawiki-tt
  "Insert truetype text."
  nil "<tt>" @ _ "</tt>" @)

(provide 'mode-mediawiki)
