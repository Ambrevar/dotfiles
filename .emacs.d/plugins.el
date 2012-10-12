;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PLUGINS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;==============================================================================
;; smart-compile
;;==============================================================================

;; (add-to-list 'load-path "~/.emacs.d/plugins/smart-compile")
;; (require 'smart-compile)

(defun my-c-mode-hook ()
  ;; (local-set-key (kbd "<f9>") (kbd "C-x C-s M-x smart-compile C-j C-j"))
  (local-set-key (kbd "<f9>") 'compile)
  )

(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'c++-mode-hook 'my-c-mode-hook)
(add-hook 'cpp-mode-hook 'my-c-mode-hook)

(require 'compile)
(add-hook 'c-mode-hook
          (lambda ()
            (unless (file-exists-p "Makefile")
              (set (make-local-variable 'compile-command)
                   ;; emulate make's .c.o implicit pattern rule, but with
                   ;; different defaults for the CC, CPPFLAGS, and CFLAGS
                   ;; variables:
                   ;; $(CC) -c -o $@ $(CPPFLAGS) $(CFLAGS) $<
                   (let ((file (file-name-nondirectory buffer-file-name)))
                     (format "%s -c -o %s.o %s %s %s"
                             (or (getenv "CC") "gcc")
                             (file-name-sans-extension file)
                             (or (getenv "CPPFLAGS") "-DDEBUG=9")
                             (or (getenv "CFLAGS") "-ansi -pedantic -Wall -Wextra -Wshadow -lm -g3 -O0")
                             file))))))

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
(yas-global-mode 1)
;; (yas/initialize)

(setq yas-snippet-dirs "~/.emacs.d/snippets" )

;; Personal snippets
;; (setq yas/root-directory "~/.emacs.d/plugins/yas/snippets" )

;; Load the snippets
;; (yas/load-directory  "~/.emacs.d/plugins/yas/snippets") ;; Warning: slow!
;; (yas/load-directory yas/root-directory)
(yas-load-directory yas-snippet-dirs)

;;==============================================================================
;; Lua
;;==============================================================================
(setq auto-mode-alist (cons '("\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)

;;==============================================================================
;; MediaWiki
;;==============================================================================

(require 'mediawiki)

;; TODO:  make it 'customize' independant.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mediawiki-site-alist (quote (("Wikipedia" "http://en.wikipedia.org/w/" "Ambrevar" "" "Main Page") ("Wikibooks" "http://en.wikibooks.org/w/" "Ambrevar" "" "LaTeX") ("ArchLinux" "https://wiki.archlinux.org/" "Ambrevar" "" "Mutt")))))


(setq mediawiki-mode-hook (lambda ()
                            (visual-line-mode 1)
                            (turn-off-auto-fill)
                            (define-key mediawiki-mode-map (kbd "C-c RET") 'mediawiki-open-page-at-point)
                            (define-key mediawiki-mode-map (kbd "C-c o") 'mediawiki-browse)
))


;;==============================================================================
;; HTML
;;==============================================================================

(add-hook 'html-mode-hook
          (lambda ()
            (turn-off-auto-fill)
            (toggle-truncate-lines)
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


