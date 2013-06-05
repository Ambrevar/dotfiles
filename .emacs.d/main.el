;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MAIN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Folders.

(setq emacs-cache-folder "~/.cache/emacs/")
(if
    (not (file-directory-p emacs-cache-folder))
    (make-directory emacs-cache-folder))

;; Remember last cursor position
(require 'saveplace)
(setq save-place-file (concat emacs-cache-folder "saveplace"))
(setq-default save-place t)

;; Disable autosave features
(setq auto-save-default nil)
(setq auto-save-list-file-prefix nil)

;; Place Backup Files in Specific Directory
;; TODO: use cache variable.
(setq backup-directory-alist
      (quote ((".*" . "~/.cache/emacs/backups/"))))
      ;; `(quote ((".*" . ,(concat emacs-cache-folder "backups/")))))
       ;; '((".*" . (concat emacs-cache-folder "backups/"))))
;; (setq backup-inhibited t) ;; Disable backup files.
;; (setq make-backup-files t) ;; Enable backup files.
;; (setq version-control t) ;; Enable numbered versioning.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; We use a minor mode to override global keys.To assign
;; global keys, you need to write
;;   (define-key my-keys-minor-mode-map (kbd "C-i") 'some-function)
(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " my-keys" 'my-keys-minor-mode-map)

(add-hook 'minibuffer-setup-hook (lambda () (my-keys-minor-mode 0) ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Default mode
(setq default-major-mode 'text-mode)

;; For convenience.
(setq inhibit-startup-screen t)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(menu-bar-mode -1)
;; (scroll-bar-mode -1)
(if (fboundp 'set-scroll-bar-mode) (set-scroll-bar-mode 'left))

;; Make questions less annoying.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Print column number.
(column-number-mode 1)

;; Kill whole line including \n.
(setq kill-whole-line t)

;; Alternative scrolling
(define-key my-keys-minor-mode-map [next]
  (lambda () (interactive)
    (condition-case nil (scroll-up)
      (end-of-buffer (goto-char (point-max))))))

(define-key my-keys-minor-mode-map [prior]
  (lambda () (interactive)
    (condition-case nil (scroll-down)
      (beginning-of-buffer (goto-char (point-min))))))

;; Line numbers
;; TODO: This mode is really messy. Replace it.
(autoload 'linum-mode "linum" "toggle line numbers on/off" t)
(if (not (fboundp 'tool-bar-mode)) (setq linum-format "%d "))
(add-hook 'find-file-hook (lambda () (linum-mode 1)))
(define-key my-keys-minor-mode-map (kbd "C-<f5>") 'linum-mode) ;; Toggle line numbers.

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

;; Highlight trailing whitespaces. For programming languages only, so that it
;; does not affect buffer like calendar and so on. There is no prog-mode-hook on
;; Emacs<24.
(mapcar
 (lambda (mode-hook)
   (add-hook
    mode-hook
    (lambda () (interactive)
      (setq show-trailing-whitespace t))))
 '(prog-mode-hook lua-mode-hook))

;; Remove whitespaces on region, or whole file.
(define-key my-keys-minor-mode-map (kbd "C-\\") 'delete-trailing-whitespace)

;; Abbreviation support
(setq default-abbrev-mode t)

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
(setq browse-url-generic-program
      (executable-find
       (let ((b (getenv "BROWSER")))
         (if b b "w3m" )))
      browse-url-browser-function 'browse-url-generic)

;; Default ispell dictionnay
;; (setq ispell-dictionary "fr")
(define-key my-keys-minor-mode-map
  (kbd "C-<f7>")
  (lambda () (interactive) (ispell-change-dictionary "en")))
(define-key my-keys-minor-mode-map
  (kbd "C-<f8>")
  (lambda () (interactive) (ispell-change-dictionary "fr")))

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

;; query-replace-regex fix on terminals.
(if (not (fboundp 'tool-bar-mode)) (define-key my-keys-minor-mode-map (kbd "C-M-y") 'query-replace-regexp))

;; Semantic options.
(semantic-mode 1)
(setq semanticdb-default-save-directory (concat emacs-cache-folder "semanticdb"))
;; Semantic with ghost display (allows M-n and M-p to browse completion).
(define-key my-keys-minor-mode-map (kbd "C-c , d") 'semantic-ia-show-summary)
;; (setq semantic-complete-inline-analyzer-displayor-class 'semantic-displayor-ghost)
;; (setq semantic-complete-inline-analyzer-displayor-class 'semantic-displayor-tooltip)
;; (setq semanticdb-find-default-throttle '(project unloaded system recursive))

;; Electric Pairs to auto-complete () [] {} "" etc. You can use it on regions.
(if (string-match "^24.*" emacs-version )
    (electric-pair-mode 1))

;; Run ranger asynchronously.
(define-key my-keys-minor-mode-map (kbd "C-x D")
  (lambda () (interactive)
    (shell-command "urxvt -e ranger &")
    (delete-windows-on "*Async Shell Command*")))

;; Calendar ISO display.
(setq calendar-week-start-day 1)
(setq calendar-date-style 'iso)

;; IDO (Interactively Do Thing) for finding files and buffers.
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

;; Quick buffer switching.
(define-key my-keys-minor-mode-map (kbd "C-<prior>") 'previous-buffer)
(define-key my-keys-minor-mode-map (kbd "C-<next>") 'next-buffer)

;; Do not open other window for buffer menu, plus hide non-file buffers.
(define-key my-keys-minor-mode-map (kbd "C-x C-b")
  (lambda () (interactive)
    (buffer-menu 1)))

;; Disable prompt (but leave warning) on git symlink.
(setq vc-follow-symlinks t)

;; Org mode config.
(add-hook 'org-mode-hook
          (lambda () (interactive) )
          (setq org-agenda-files '("~/todo.org"))
          (setq org-enforce-todo-dependencies t))

;; Ediff in one frame.
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Change GUD many-windows layout.
(defun gdb-setup-windows ()
  "Layout the window pattern for `gdb-many-windows'."
  (gdb-display-locals-buffer)
  (gdb-display-stack-buffer)
  (delete-other-windows)
  (gdb-display-breakpoints-buffer)
  (delete-other-windows)
  (switch-to-buffer
   (if gud-last-last-frame
       (gud-find-file (car gud-last-last-frame))
     (if gdb-main-file
         (gud-find-file gdb-main-file)
       ;; Put buffer list in window if we
       ;; can't find a source file.
       (list-buffers-noselect))))
  (setq gdb-source-window (selected-window))
  (split-window-horizontally)
  (other-window 1)
  (split-window nil ( / ( * (window-height) 3) 4))
  (split-window nil ( / (window-height) 3))
  (gdb-set-window-buffer (gdb-locals-buffer-name))
  (other-window 1)
  (pop-to-buffer gud-comint-buffer)
  (when gdb-use-separate-io-buffer
    (split-window-horizontally)
    (other-window 1)
    (gdb-set-window-buffer
     (gdb-get-buffer-create 'gdb-inferior-io)))
  (other-window 1)
  (gdb-set-window-buffer (gdb-stack-buffer-name))
  (split-window-horizontally)
  (other-window 1)
  (gdb-set-window-buffer (gdb-breakpoints-buffer-name))
  (other-window 1))
;; Set GUD to display many windows by default.
(setq gdb-many-windows t)

;; Support for dwb edit.
(add-hook
 'find-file-hook
 (lambda ()
   (if (string-match "edit*" (buffer-name))
       (auto-fill-mode -1))))

