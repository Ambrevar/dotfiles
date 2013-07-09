;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MAIN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Remember last cursor position.
(require 'saveplace)
(setq save-place-file (concat emacs-cache-folder "saveplace"))
(setq-default save-place t)

;; Disable autosave features.
(setq auto-save-default nil)
(setq auto-save-list-file-prefix nil)

;; Place backup files in specific directory.
(setq backup-directory-alist
       `((".*" . ,(concat emacs-cache-folder "backups/"))))

;; Other backup options.
; (setq backup-inhibited t) ;; Disable backup files.
; (setq make-backup-files t) ;; Enable backup files.
; (setq version-control t) ;; Enable numbered versioning.

;; Default mode
(setq default-major-mode 'text-mode)

;; Disable suspend key since it is useless on Emacs server.
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;; For convenience.
(setq inhibit-startup-screen t)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(menu-bar-mode -1)
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode 'left)
  (scroll-bar-mode -1)
  (define-key my-keys-minor-mode-map (kbd "C-<f6>") 'toggle-scroll-bar))

;; Make questions less annoying.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Print column number in mode line.
(column-number-mode 1)

;; Print buffer size in mode line.
(size-indication-mode 1)

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
;; TODO: This mode is slow on  big files when using beginning-of-buffer binding.
(autoload 'linum-mode "linum" "toggle line numbers on/off" t)
(add-hook 'find-file-hook (lambda () (linum-mode 1)))
(define-key my-keys-minor-mode-map (kbd "C-<f5>") 'linum-mode)
(add-hook
 'linum-before-numbering-hook
 (lambda () (if (display-graphic-p) (setq linum-format "%d") (setq linum-format "%d "))))

;; Indentation
(setq-default tab-width 4)
(setq-default standard-indent 4)
(setq-default c-basic-offset 4)
(setq-default indent-tabs-mode nil) ;; Indentation cannot insert tabs

;; Line by line scrolling
(setq scroll-step 1)

;; Highlight selections -- not activated by default on old Emacs.
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

;; Set  man pages to display on a 80 character wide window.
(setenv "MANWIDTH" "80")

;; Windmove mode: easy window switching with Shift+arrows.
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; Make Emacs use environment browser, or w3m if BROWSER is not set.
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
(if (>= emacs-major-version 24)
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
(setq ido-save-directory-list-file (concat emacs-cache-folder "ido.last"))
;; All file finding operation defaults to what is at point. Incompatible with
;; IDO.
;; (ffap-bindings)

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
;; Move annoying babel folder. This move does not seem to work properly.
(setq org-babel-temporary-directory (concat emacs-cache-folder "babel"))
(add-hook
 'org-mode-hook
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

     ;; TODO: this does not behave the same on Emacs 23 and 24.
     (switch-to-buffer
      (if gud-last-last-frame
          (gud-find-file (car gud-last-last-frame))
        (if gdb-main-file
            (gud-find-file gdb-main-file)
          ;; Put buffer list in window if we can't find a source file.
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

;; Compilation .
(setq compilation-hide-window nil)
(define-key my-keys-minor-mode-map (kbd "<f10>")
  (lambda () (interactive)
    (save-buffer)
    (compile compile-command)
    (when compilation-hide-window
      (sit-for 2)
      (delete-windows-on "*compilation*"))))
(define-key my-keys-minor-mode-map (kbd "<f11>") 'previous-error)
(define-key my-keys-minor-mode-map (kbd "<f12>") 'next-error)
;; Code browsing
(define-key my-keys-minor-mode-map (kbd "C-M-e") (lambda () (interactive) (beginning-of-defun -1)))

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

(define-key my-keys-minor-mode-map (kbd "C-<f10>")
  'flymake-display-err-message-for-current-line)

;; Ediff split horizontally by default.
;; TODO: does not seem to work.
(add-hook
 'ediff-mode-hook
 (lambda ()
   (setq ediff-merge-split-window-function 'split-window-horizontally)))

;; Shell allow comment indenation.
(setq sh-indent-comment t)
;; Eshell
(setq eshell-directory-name (concat emacs-cache-folder "eshell"))
;; We do not put 'sh' only because it could get messy. Emacs knows it anyway.
(add-to-list 'auto-mode-alist '("\\(bash\\'\\|zsh\\'\\|csh\\'\\|tcsh\\'\\|ksh\\'\\)" . sh-mode))
(add-to-list 'auto-mode-alist '("rc\\'" . sh-mode))

;; GLSL fallback to C mode.
(add-to-list 'auto-mode-alist '("\\.vert\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.glsl\\'" . c-mode))
(autoload 'glsl-mode "glsl-mode" nil t)
(when (boundp 'glsl-mode)
  (add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode)))

;; Lua
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))
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

;; Bison/flex -- Fallback to c-mode.
(add-to-list 'auto-mode-alist '("\\.yy?\\'" . c-mode)))
(add-to-list 'auto-mode-alist '("\\.l\\'" . c-mode)))
(if (require 'bison-mode nil t)
    (add-to-list 'auto-mode-alist '("\\.yy?\\'" . bison-mode)))
(if (require 'flex-mode nil t)
    (add-to-list 'auto-mode-alist '("\\.l\\'" . flex-mode)))

;; Multiple-Cursors
;; (add-to-list 'load-path "/usr/share/emacs/site-lisp/multiple-cursors")
(if (require 'multiple-cursors nil t)
    (progn
      (setq mc/list-file (concat emacs-cache-folder "mc-lists.el"))
      (global-unset-key (kbd "C-<down-mouse-1>"))
      (define-key my-keys-minor-mode-map (kbd "C-<mouse-1>") 'mc/add-cursor-on-click)
      (define-key my-keys-minor-mode-map (kbd "C-c C-r") 'mc/edit-lines)
      (define-key my-keys-minor-mode-map (kbd "C-c C-m") 'mc/mark-more-like-this-extended)
      (define-key my-keys-minor-mode-map (kbd "C-c C-l") 'mc/mark-all-like-this-dwim)))

;; Let Emacs auto-load/save sessions.
(when (boundp 'server-running-p)
  (desktop-save-mode 1)
  (setq history-length 250)
  (setq desktop-dirname (concat emacs-cache-folder "desktop"))
  (if (not (file-directory-p desktop-dirname))
      (make-directory desktop-dirname t))
  (setq desktop-path `(,desktop-dirname))
  (add-to-list 'desktop-globals-to-save 'compile-command))

(defadvice pop-to-buffer (before cancel-other-window first)
  (ad-set-arg 1 nil))

(ad-activate 'pop-to-buffer)

;; Toggle window dedication
(defun toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not"
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window
                                 (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))

;; Press [pause] key in each window you want to "freeze"
(global-set-key [pause] 'toggle-window-dedicated)

;; Dired options
;; On a GNU system, ls has the option to sort folders first.
(if (string-match "^gnu.*" (prin1-to-string system-type))
    (setq dired-listing-switches "--group-directories-first -lh")
  (setq dired-listing-switches "-lh"))
(setq wdired-allow-to-change-permissions t)

(defvar dired-showing-hidden nil "If dired is displaying hidden files or not.")
(defvar dired-showing-humansize t "If dired is displaying humansize or not.")

(defun dired-toggle-hidden ()
  "Toggle displaying hidden files in dired."
  (interactive)
  (let (;; Regexp for finding (possibly embedded) -a switches.
        (switch-regexp "\\(\\`\\| \\)-\\([b-zA-Z]*\\)\\(a\\)\\([^ ]*\\)")
        case-fold-search)
    ;; Remove the -a switch.
    (while (string-match switch-regexp dired-actual-switches)
      (if (and (equal (match-string 2 dired-actual-switches) "")
               (equal (match-string 4 dired-actual-switches) ""))
          ;; Remove a stand-alone -a switch.
          (setq dired-actual-switches
                (replace-match "" t t dired-actual-switches))
        ;; Remove a switch of the form -XaY for some X and Y.
        (setq dired-actual-switches
              (replace-match "" t t dired-actual-switches 3))))
    ;; Now, if we weren't sorting by date before, add the -a switch.  Some
    ;; simple-minded ls implementations (eg ftp servers) only allow a single
    ;; option string, so try not to add " -a" if possible.
    (if dired-showing-hidden
        (setq dired-showing-hidden nil)
      (progn
        (setq dired-actual-switches
              (concat dired-actual-switches
                      (if (string-match-p "\\`-[[:alnum:]]+\\'"
                                          dired-actual-switches)
                          "a" " -a")))
        (setq dired-showing-hidden t))))
  ;; (dired-sort-set-mode-line)
  (revert-buffer))

(defun dired-toggle-humansize ()
  "Toggle displaying humansize in dired."
  (interactive)
  (let ((switch-regexp "\\(\\`\\| \\)-\\([a-gi-zA-Z]*\\)\\(h\\)\\([^ ]*\\)")
        case-fold-search)
    (while (string-match switch-regexp dired-actual-switches)
      (if (and (equal (match-string 2 dired-actual-switches) "")
               (equal (match-string 4 dired-actual-switches) ""))
          (setq dired-actual-switches
                (replace-match "" t t dired-actual-switches))
        (setq dired-actual-switches
              (replace-match "" t t dired-actual-switches 3))))
    (if dired-showing-humansize
        (setq dired-showing-humansize nil)
      (progn
        (setq dired-actual-switches
              (concat dired-actual-switches
                      (if (string-match-p "\\`-[[:alnum:]]+\\'"
                                          dired-actual-switches)
                          "h" " -h")))
        (setq dired-showing-humansize t))))
  (revert-buffer))

(add-hook
 'dired-mode-hook
 (lambda ()
   (local-set-key (kbd "C-c a") 'dired-toggle-hidden)
   (local-set-key (kbd "C-c h") 'dired-toggle-humansize)
   ;; (local-set-key (kbd "<left>") 'dired-up-directory)
   ;; (local-set-key (kbd "<right>") 'dired-find-file)
   (local-set-key (kbd "b") 'dired-up-directory)))

;; Bookmark file to cache folder
(setq bookmark-default-file (concat emacs-cache-folder "emacs.bmk"))

;; Read Matlab files in Octave mode.
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;; Mutt support.
(add-to-list 'auto-mode-alist '("/tmp/mutt.*" . mail-mode))

;; Arch Linux PKGBUILD.
(add-to-list 'auto-mode-alist '("PKGBUILD" . sh-mode))

;; README files.
(add-to-list 'auto-mode-alist '("README" . text-mode))

;; Subtitles support.
(add-to-list 'auto-mode-alist '("\\.srt\\'" . text-mode))

;; Git commit meessages.
(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG\\'" . conf-mode))
