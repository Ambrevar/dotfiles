;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MAIN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Make questions less annoying
(defalias 'yes-or-no-p 'y-or-n-p)

;; Print column number
(column-number-mode 1)

;; Kill whole line
(setq kill-whole-line t)

;; Modern scrolling
(global-set-key [next]
                (lambda () (interactive)
                  (condition-case nil (scroll-up)
                    (end-of-buffer (goto-char (point-max))))))

(global-set-key [prior]
                (lambda () (interactive)
                  (condition-case nil (scroll-down)
                    (beginning-of-buffer (goto-char (point-min))))))

;; Line numbers
(autoload 'linum-mode "linum" "toggle line numbers on/off" t)
;; (setq linum-format "%-4d ")
(setq linum-format "%d ")
(global-linum-mode 1) ;; FIXME: This may generate warnings with emacsclient. Bug?
(global-set-key (kbd "C-<f5>") 'linum-mode) ;; Toggle line numbers.

;; Indentation
;(setq standard-indent 4)
(setq-default tab-width 4) 
(setq-default indent-tabs-mode nil) ;; Indentation cannot insert tabs

;; Line by line scrolling
(setq scroll-step 1)

;; Highlight selections -- not activated by default on old emacs.
(transient-mark-mode 1)

;; No trailing whitespace
;; WARNING: this can break some configuration files needing whitespaces at the
;; end.
;; (add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Abbreviation support
(setq default-abbrev-mode t)

;; Remember last cursor position
(require 'saveplace)
(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)

;; Disable autosave features
(setq auto-save-default nil)

;; Place Backup Files in Specific Directory
;; (setq backup-inhibited t) ;; Disable backup files.
;; (setq make-backup-files t) ;; Enable backup files.
;; (setq version-control t) ;; Enable numbered versioning.
(setq backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/")))) ;; Save all backup file in this directory.

;; Remove menu-bar
(menu-bar-mode -1)

;; Set Fill Column
(setq-default fill-column 80)
;; (auto-fill-mode 1) ;; Will not work because it gets overridden by major modes.
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Man-mode
(setenv "MANWIDTH" "80")

;; Windmove mode
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; Browser
(setq browse-url-generic-program (executable-find "dwb")
browse-url-browser-function 'browse-url-generic)

;; Default ispell dictionnay
(setq ispell-dictionary "fr")

;; Use color escape sequences. Only use if needed.
;; (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Long paragraphs. Useful for quick navigation with backward-paragraph and
;; forward-paragraph.
(setq paragraph-start "
")

;; Show matching parenthesis
(show-paren-mode 1)
;; By default, there’s a small delay before showing a matching parenthesis. Set
;; it to 0 to deactivate.
(setq show-paren-delay 0)

;; Window resize
;; TODO: Use more { and } to continue.
(global-set-key (kbd "C-x {")  (lambda () (interactive) (shrink-window-horizontally 5)))
(global-set-key (kbd "C-x }")  (lambda () (interactive) (enlarge-window-horizontally 5)))
;; (define-key my-keys-minor-mode-map (kbd "S-C-<down>") 'shrink-window)
;; (define-key my-keys-minor-mode-map (kbd "S-C-<up>") 'enlarge-window)

;; Copy/Paste to/from clipboard.
;; FIXME: copying does not work.
(global-set-key (kbd "C-<f6>") (kbd "M-| xclip"))
(global-set-key (kbd "C-<f7>") (kbd "C-u M-! xclip <SPC> -o"))
(global-set-key (kbd "C-<f8>") (kbd "C-u M-! xclip <SPC> -o <SPC> -selection <SPC> clipboard"))

;; query-replace-regex fix on terminals.
(global-set-key (kbd "C-M-y") 'query-replace-regexp)
