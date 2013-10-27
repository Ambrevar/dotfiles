;;==============================================================================
;; MediaWiki
;;==============================================================================

(when (require 'mediawiki nil t)

  (setq mediawiki-site-alist
        '(
          ("Wikipedia" "http://en.wikipedia.org/w/" "Ambrevar" "" "Main Page")
          ("Wikibooks" "http://en.wikibooks.org/w/" "Ambrevar" "" "LaTeX")
          ("ArchLinux" "https://wiki.archlinux.org/" "Ambrevar" "" "Mutt")
          ("WikEmacs" "https://wikemacs.org/wiki/" "Ambrevar" "" "Main Page")
          ))

  ;; The url-cookie timer is set to a high value because it seems that once the
  ;; cookie has been saved, MediaWiki fails to upload files correctly.  Is
  ;; 'url-do-setup' needed to make sure the url-cookie-save-interval variable is
  ;; taken into account?
  ;; TODO: url-cookie timer does not work.
  (setq url-cookie-save-interval 86400)

  (setq mediawiki-mode-hook
        (lambda ()
          (visual-line-mode 1)
          (turn-off-auto-fill)
          (define-key mediawiki-mode-map (kbd "C-c RET") 'mediawiki-open-page-at-point)
          (define-key mediawiki-mode-map (kbd "C-c o") 'mediawiki-browse)
          (local-unset-key (kbd "M-g"))
          (local-set-key (kbd "C-c C-d") 'duplicate-line)
          (local-set-key (kbd "C-c C-s") 'mediawiki-save)
          (local-set-key (kbd "C-x C-s") 'save-buffer)
          )) )
