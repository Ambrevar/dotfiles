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

;; Macros.
;; Use C-( stuff C-) to record a macro.
;; Use 'name-last-kbd-macro' to give it a name.
;; Use 'insert-kbd-macro' in your init file to insert the code.
;; You can assign a key: (local-set-key (kbd "C-c a") 'my-macro)
(fset 'ltx-template-source-to-latex
   "\C-@\C-s>\C-m\C-w\C-@\C-s</source\C-m\C-[Od\C-[[D\C-[[D\C-wltx\C-i\C-y\C-i\C-[d\C-d")

(fset 'ltx-template-source-to-usage
      "\C-@\C-s>\C-w\C-?\C-m\C-w\C-d\C-@\C-s</source\C-m\C-[Od\C-[[D\C-[[D\C-[[D\C-wltxu\C-i\C-y\C-i\C-k\C-k")

(setq mediawiki-site-alist
      '(
        ("Wikipedia" "http://en.wikipedia.org/w/" "Ambrevar" "" "Main Page")
        ("Wikibooks" "http://en.wikibooks.org/w/" "Ambrevar" "" "LaTeX")
        ("ArchLinux" "https://wiki.archlinux.org/" "Ambrevar" "" "Mutt"))
      )

;; The url-cookie timer is set to a high value because it seems that once the
;; cookie has been saved, MediaWiki fails to upload files correctly.  Is
;; 'url-do-setup' needed to make sure the url-cookie-save-interval variable is
;; taken into account? TODO: does not work.
(setq url-cookie-save-interval 86400)

(setq mediawiki-mode-hook
      (lambda ()
        (visual-line-mode 1)
        (turn-off-auto-fill)
        (define-key mediawiki-mode-map (kbd "C-c RET") 'mediawiki-open-page-at-point)
        (define-key mediawiki-mode-map (kbd "C-c o") 'mediawiki-browse)
        (local-unset-key (kbd "M-g"))
        ;; TODO: Bindings to not work???
        ;; (local-set-key (kbd "C-c l L") 'ltx-template-source-to-latex)
        ;; (local-set-key (kbd "C-c l U") 'ltx-template-source-to-usage)
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
;; Zlc
;;==============================================================================
;; Zsh style completion.

(require 'zlc)
(let ((map minibuffer-local-map))
  ;; Like Zsh menu select.  Should not use arrows directly because it overrides
  ;; default controls like previous entry, or previous/next char.
  (define-key map (kbd "C-<down>")  'zlc-select-next-vertical)
  (define-key map (kbd "C-<up>")    'zlc-select-previous-vertical)
  (define-key map (kbd "C-<right>") 'zlc-select-next)
  (define-key map (kbd "C-<left>")  'zlc-select-previous)

  ;;; Reset selection.
  (define-key map (kbd "C-c") 'zlc-reset)
  )

;; (setq zlc-select-completion-immediately t)

;; To change style, M-x customize-face and input zlc-selected-completion-face.
