;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MAIN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rc files support
(setq auto-mode-alist (append '(("rc\\'" . sh-mode)) auto-mode-alist))

;; Shell support
;; We do not put 'sh' only because it could get messy. Emacs knows it anyway.
(setq auto-mode-alist
      (append
       '(("\\(bash\\'\\|zsh\\'\\|csh\\'\\|tcsh\\'\\|ksh\\'\\)" . sh-mode))
       auto-mode-alist))

;; Read Matlab files in Octave mode.
(setq auto-mode-alist (append '(("\\.m\\'" . octave-mode)) auto-mode-alist))

;; Read pl files in prolog mode.
;; WARNING: this extension is shared with Perl.
;; (setq auto-mode-alist (append '(("\\.pl\\'" . prolog-mode)) auto-mode-alist))

;; Mutt support.
(setq auto-mode-alist (append '(("/tmp/mutt.*" . mail-mode)) auto-mode-alist))

;; Arch Linux PKGBUILD.
(setq auto-mode-alist (append '(("PKGBUILD" . sh-mode)) auto-mode-alist))

;; README files.
(setq auto-mode-alist (append '(("README" . text-mode)) auto-mode-alist))

;; Lex/Flex + Yacc/Bison mode fallback to c-mode.
(setq auto-mode-alist (append '(("\\.l\\'" . c-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.yy?\\'" . c-mode)) auto-mode-alist))

;; Subtitles support.
(setq auto-mode-alist (append '(("\\.srt\\'" . text-mode)) auto-mode-alist))

;; Git commit meessages.
(setq auto-mode-alist (append '(("COMMIT_EDITMSG\\'" . conf-mode)) auto-mode-alist))

;;==============================================================================

;; Remember last cursor position
(require 'saveplace)
(setq save-place-file (concat emacs-cache-folder "saveplace"))
(setq-default save-place t)

;; Disable autosave features
(setq auto-save-default nil)
(setq auto-save-list-file-prefix nil)

;; Place Backup Files in Specific Directory
(setq backup-directory-alist
       `((".*" . ,(concat emacs-cache-folder "backups/"))))

;; Other backup options.
; (setq backup-inhibited t) ;; Disable backup files.
; (setq make-backup-files t) ;; Enable backup files.
; (setq version-control t) ;; Enable numbered versioning.

;; Default mode
(setq default-major-mode 'text-mode)

;; Disable suspend key since it is useless on emacs server.
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;; For convenience.
(setq inhibit-startup-screen t)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(menu-bar-mode -1)
(when (fboundp 'set-scroll-bar-mode)
  ;; (scroll-bar-mode -1)
  (set-scroll-bar-mode 'left)
  (define-key my-keys-minor-mode-map (kbd "C-<f6>") 'toggle-scroll-bar))

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
(setq-default tab-width 4)
(setq-default standard-indent 4)
(setq-default c-basic-offset 4)
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
(define-key my-keys-minor-mode-map (kbd "C-c , d") 'semantic-ia-show-summary)
(define-key my-keys-minor-mode-map (kbd "C-, d") 'semantic-ia-show-summary)
(define-key my-keys-minor-mode-map (kbd "C-, g") 'semantic-symref-symbol)
(define-key my-keys-minor-mode-map (kbd "C-, G") 'semantic-symref)
(define-key my-keys-minor-mode-map (kbd "C-, j") 'semantic-complete-jump-local)
(define-key my-keys-minor-mode-map (kbd "C-, J") 'semantic-complete-jump)
(define-key my-keys-minor-mode-map (kbd "C-, l") 'semantic-analyze-possible-completions)
;; Semantic with ghost display (allows M-n and M-p to browse completion).
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

;; IDO (Interactively Do Thing) for finding files and buffers. Incompatible with
;; FFAP.
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)
;; All file finding operation defaults to what is at point. Incompatible with
;; IDO.  (ffap-bindings)

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
          (lambda () (interactive)
          (setq org-agenda-files '("~/todo.org"))
          (setq org-enforce-todo-dependencies t)))
;; Set PDF association in Org-mode (was Evince by default).
(eval-after-load "org"
  '(progn
     ;; Change .pdf association directly within the alist
     (setcdr (assoc "\\.pdf\\'" org-file-apps) "zathura --fork %s")))

;; Ediff in one frame.
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Set GUD to display many windows by default.
;; (setq gdb-show-main t)
(setq gdb-many-windows t)
;; Change GUD many-windows layout.
(add-hook
 'gud-mode-hook
 (lambda () (interactive)
   (defun gdb-setup-windows ()
     "Layout the window pattern for `gdb-many-windows'."
     (setq gdb-source-window (selected-window))
     (gdb-display-locals-buffer)
     (delete-other-windows)
     (gdb-display-stack-buffer)
     (delete-other-windows)
     (gdb-display-breakpoints-buffer)
     (delete-other-windows)

     ;; TODO: this does not behave the same on Emacs 23 and 24
     (switch-to-buffer
      (if gud-last-last-frame
          (gud-find-file (car gud-last-last-frame))
        (if gdb-main-file
            (gud-find-file gdb-main-file)
          ;; Put buffer list in window if we
          ;; can't find a source file.
          (list-buffers-noselect))))

     (split-window-horizontally)
     (other-window 1)
     (split-window nil ( / ( * (window-height) 3) 4))
     (split-window nil ( / (window-height) 3))
     (gdb-set-window-buffer (gdb-locals-buffer-name))
     (other-window 1)
     (gdb-set-window-buffer gud-comint-buffer)
     (when (and
            (boundp 'gdb-use-separate-io-buffer)
            gdb-use-separate-io-buffer)
       (split-window-horizontally)
       (other-window 1)
       (gdb-set-window-buffer
        (gdb-get-buffer-create 'gdb-inferior-io)))
     (other-window 1)
     (gdb-set-window-buffer (gdb-stack-buffer-name))
     (split-window-horizontally)
     (other-window 1)
     (gdb-set-window-buffer (gdb-breakpoints-buffer-name))
     (other-window 1))))

;; Remove auto-fill in dwb edit because wikis and forums do not like it.
(add-hook
 'find-file-hook
 (lambda ()
   (if (string-match "edit*" (buffer-name))
       (auto-fill-mode -1))))

;; Speedbar options.
(add-hook
 'speedbar-mode-hook
 (lambda ()
   (speedbar-toggle-updates)))

;; Compilation error report.
(define-key my-keys-minor-mode-map (kbd "<f12>") 'next-error)
(define-key my-keys-minor-mode-map (kbd "<f11>") 'previous-error)

;; Just because XML is ugly.
(add-hook
 'html-mode-hook
 (lambda ()
   (turn-off-auto-fill)
   (toggle-truncate-lines)))

;; Common LISP
(setq inferior-lisp-program "clisp")

;; Flymake has a bug that prevents menu from spawning in a console. We redefine
;; the function to spawn the error message in the mini-buffer.
(defun flymake-display-err-message-for-current-line ()
  "Display a message with errors/warnings for current line if it
has errors and/or warnings."
  (interactive)
  (let* ((line-no             (flymake-current-line-no))
         (line-err-info-list  (nth 0 (flymake-find-err-info flymake-err-info line-no)))
         (menu-data           (flymake-make-err-menu-data line-no line-err-info-list)))
    (if menu-data
        (let ((messages))
          (push (concat (car menu-data) ":") messages)
          (dolist (error-or-warning (cadr menu-data))
            (push (car error-or-warning) messages))
          (message "%s" (mapconcat #'identity (reverse messages) "\n"))))))

(global-set-key (kbd "<f10>")
  'flymake-display-err-message-for-current-line)

;; Ediff split horizontally by default.
;; TODO: does not seem to work.
(add-hook
 'ediff-mode-hook
 (lambda ()
   (setq ediff-merge-split-window-function 'split-window-horizontally)))

;; Shell allow comment indenation.
(setq sh-indent-comment t)

;; GLSL support.
(autoload 'glsl-mode "glsl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))

;; Lua
(setq auto-mode-alist (cons '("\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)

;; Zlc - Zsh style completion.
;; (if (require 'zlc nil t)
;;     (let ((map minibuffer-local-map))
;;       ;; Like Zsh menu select.  Should not use arrows directly because it overrides
;;       ;; default controls like previous entry, or previous/next char.
;;       (define-key map (kbd "M-<down>")  'zlc-select-next-vertical)
;;       (define-key map (kbd "M-<up>")    'zlc-select-previous-vertical)
;;       (define-key map (kbd "M-<right>") 'zlc-select-next)
;;       (define-key map (kbd "M-<left>")  'zlc-select-previous)
;;       ;; Reset selection.
;;       (define-key map (kbd "C-c") 'zlc-reset)
;;       ;; (setq zlc-select-completion-immediately t)
;;       ;; To change style, M-x customize-face and input zlc-selected-completion-face.
;;       ))

;; xclip
(if (require 'xclip nil t)
    (turn-on-xclip))

;; Bison/flex
(if (require 'bison-mode nil t)
    (add-to-list 'auto-mode-alist '("\\.yy?\\'" . bison-mode)))
(if (require 'flex-mode nil t)
    (add-to-list 'auto-mode-alist '("\\.l\\'" . flex-mode)))

;; Multiple-Cursors
;; (add-to-list 'load-path "/usr/share/emacs/site-lisp/multiple-cursors")
(if (require 'multiple-cursors nil t)
    (progn
      (setq mc/list-file "~/.emacs.d/mc-lists.el")
      (define-key my-keys-minor-mode-map (kbd "C-c C-a") 'mc/edit-beginnings-of-lines)
      (define-key my-keys-minor-mode-map (kbd "C-c C-e") 'mc/edit-ends-of-lines)
      (define-key my-keys-minor-mode-map (kbd "C-c C-r") 'mc/edit-lines)
      (define-key my-keys-minor-mode-map (kbd "C-c C-n") 'mc/mark-next-like-this)
      (define-key my-keys-minor-mode-map (kbd "C-c C-p") 'mc/mark-previous-like-this)
      (define-key my-keys-minor-mode-map (kbd "C-c C-l") 'mc/mark-all-like-this-dwim)))


;; Let Emacs auto-load/save sessions.
(desktop-save-mode 1)
(setq history-length 250)
(add-to-list 'desktop-globals-to-save 'compile-command)
