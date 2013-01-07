;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PLUGINS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; User plugin path.
(add-to-list 'load-path "~/.emacs.d/plugins")

;;==============================================================================
;; Yasnippet
;;==============================================================================
;; Yasnippet is slow when loading snippets from source.
;; Generate a bundle instead: yas/compile-bundle
;; Besides you can convert the generated file to bytecode.

;; (add-to-list 'load-path "~/.emacs.d/plugins")
;; (require 'yasnippet-bundle)

;; Next follows a traditional, yet not-optimized configuration for Yasnippet.
;; (add-to-list 'load-path "/usr/share/emacs/site-lisp/yas")
(require 'yasnippet) ;; not yasnippet-bundle

(setq yas-snippet-dirs "~/.emacs.d/snippets" )

(if (fboundp 'yas/initialize)
    ;; Old yasnippet versions.
    (progn
      (yas/initialize)
      (yas/load-directory yas-snippet-dirs))
  
  ;; New yasnippet versions.
  (progn
   (yas-global-mode 1)
   (yas-load-directory yas-snippet-dirs))
)


;;==============================================================================
;; Lua
;;==============================================================================
(setq auto-mode-alist (cons '("\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)

;;==============================================================================
;; MediaWiki
;;==============================================================================

(require 'mediawiki)

;; TODO: make it 'customize' independant. 

;; The url-cookie timer is set to a high value because it seems that once the
;; cookie is saved, MediaWiki fails to upload files correctly. TODO: does not
;; work.
(custom-set-variables
 '(mediawiki-site-alist 
   (quote
    (
     ("Wikipedia" "http://en.wikipedia.org/w/" "Ambrevar" "" "Main Page")
     ("Wikibooks" "http://en.wikibooks.org/w/" "Ambrevar" "" "LaTeX")
     ("ArchLinux" "https://wiki.archlinux.org/" "Ambrevar" "" "Mutt"))
    ))
 '(url-cookie-save-interval 86400)
 )

(setq mediawiki-mode-hook
      (lambda ()
        (visual-line-mode 1)
        (turn-off-auto-fill)
        (define-key mediawiki-mode-map (kbd "C-c RET") 'mediawiki-open-page-at-point)
        (define-key mediawiki-mode-map (kbd "C-c o") 'mediawiki-browse)
))

;;==============================================================================
;; DoxyMacs
;;==============================================================================

;; (require 'doxymacs)
;; (add-hook 'c-mode-common-hook 'doxymacs-mode)

;; ;; Fontified comments.
;; (defun my-doxymacs-font-lock-hook ()
;;   (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
;;       (doxymacs-font-lock)))
;; (add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)


;;==============================================================================
;; Auto-Complete
;;==============================================================================
;; (add-to-list 'load-path "/usr/share/emacs/site-lisp/auto-complete")
;; (require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories "/usr/share/emacs/site-lisp/auto-complete/ac-dict")
;; (ac-config-default)


