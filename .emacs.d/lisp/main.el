;;; Main options

(require 'functions)

;;; Minimal UI. Run early to hide it as soon as possible.
(setq inhibit-startup-screen t)
(menu-bar-mode -1)
;;; `tool-bar-mode' and `scroll-bar-mode' might not be compiled in.
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;;; In some cases, Emacs can still decide by itself to use graphical boxes.
;;; Force on using the minibuffer instead.
(setq use-dialog-box nil)

;;; Timeout before echoing the prefix of an unfinished keystroke.
(setq echo-keystrokes 0.5)

;;; Remember last cursor position.
(setq save-place-file (concat emacs-cache-folder "saveplace"))
(setq-default save-place-mode t)
;;; When the daemon is killed abruptly, places are not saved. Adding this hook
;;; allows to save places at a strategic moment.
(add-hook 'before-save-hook 'save-place-kill-emacs-hook)

;;; Network files
(setq url-cookie-file (concat emacs-cache-folder "url.cookies"))
(with-eval-after-load 'nsm
  (setq nsm-settings-file (concat emacs-cache-folder "network-security.data")))

;;; Bookmark file to cache folder.
(setq bookmark-default-file (concat emacs-cache-folder "emacs.bmk"))

;;; Recent files.
(setq recentf-save-file (concat emacs-cache-folder "recentf")
      recentf-max-saved-items 40)

;;; Save M-: history.
(setq savehist-file (concat emacs-cache-folder "savehist"))
(savehist-mode)

;;; Disable autosave features.
(setq auto-save-default nil)
(setq auto-save-list-file-prefix nil)

;;; Place backup files in specific directory.
(setq backup-directory-alist
      `(("." . ,(concat emacs-cache-folder "backups/"))))

;;; Default mode
(setq-default major-mode 'text-mode)

;;; Disable suspend key since it is useless on Emacs server.
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;;; Make questions less annoying.
(defalias 'yes-or-no-p 'y-or-n-p)

;;; Enable all disabled commands.
(setq disabled-command-function nil)

;;; Print buffer size in mode line.
(size-indication-mode 1)

;;; Display defun in mode line.
;; (which-function-mode)

;;; No need when we have a status bar.
;; (display-time)
;; (setq display-time-day-and-date t
;;       display-time-24hr-format t
;;       display-time-default-load-average nil)

;;; Just like time, no need when we have a status bar.
;; (display-battery-mode)
;;; TODO: Battery status (%b) does not work properly.
;; (setq battery-mode-line-format "[%p%%%b %t]")

;;; Line numbers
;;; Adding to `find-file-hook' ensures it will work for every file, regardless of
;;; the mode, but it won't work for buffers without files nor on mode change.
(dolist (hook '(prog-mode-hook text-mode-hook))
  (add-hook hook 'turn-on-column-number-mode)
  (add-hook hook 'turn-off-line-number-mode)
  (add-hook hook 'linum-mode))
;;; Emacs-nox does not display a fringe after the linum: Setting linum-format in
;;; linum-before-numbering-hook is not the right approach as it will change the
;;; type of linum-format in the middle. See linum-update-window.
;;; See http://stackoverflow.com/questions/3626632/right-align-line-numbers-with-linum-mode
;;; and http://stackoverflow.com/questions/3626632/right-align-line-numbers-with-linum-mode.
;;; The complexity is not worth the benefit.

;;; Alternative scrolling
(setq scroll-error-top-bottom t)

;;; Kill whole line including \n.
(setq kill-whole-line t)

;;; Indentation
(setq-default tab-width 2)
(defvaralias 'standard-indent 'tab-width)
(setq-default indent-tabs-mode t)

;;; Line by line scrolling
(setq scroll-step 1)

(global-set-key (kbd "<f9>") 'whitespace-mode)
(setq
 whitespace-style
 '(face empty indentation space-after-tab space-before-tab tab-mark trailing))
;;; REVIEW: `whitespace-report' will mistakenly always report empty lines at
;;; beginning and end of buffer as long as there is at least one empty line.
;;; `whitespace-cleanup' works properly however.
;;; Reported at http://debbugs.gnu.org/cgi/bugreport.cgi?bug=23740.
;; (setq whitespace-action '(report-on-bogus))

;;; Add formatting functions to the buffer-local `before-save-hook'.
;;; WARNING: this can break some configuration files needing whitespaces at the
;;; end. This can also slow down saving on big files.  Some modes (e.g. lisp) run
;;; `fmt' in their local hook, which is redundant with this.
;; (add-hook 'find-file-hook 'turn-on-fmt-before-save)
(add-hook 'find-file-hook 'turn-on-delete-trailing-whitespace)

;;; Hippie expand
;; (global-set-key (kbd "M-/") 'hippie-expand)

;;; Abbreviation is like snippets: annoying at times, especially in
;;; prog-mode.  They are useful in text mode to avoid the sprawling of
;;; abbreviations.
(add-hook 'text-mode-hook 'abbrev-mode)

;;; Auto-fill
(when (getenv "MANWIDTH")
  (setq-default fill-column (string-to-number (getenv "MANWIDTH"))))
(add-hook 'text-mode-hook 'turn-on-auto-fill)
;; (setq sentence-end-double-space nil)

;;; Enforce horizontal splitting. 140 means that the window is large enough to
;;; hold 2 other windows of 70 columns.
(setq split-height-threshold nil
      split-width-threshold 140)

;;; Windmove mode
;; By default, it allows easy window switching with Shift+arrows. I like to
;; stick to the home-row, but to avoid shadowing other binding I exceptionaly use
;; 'super' (normally reserved to the WM).
(when (fboundp 'windmove-default-keybindings)
  (global-set-keys
   "s-h" 'windmove-left
   "s-j" 'windmove-down
   "s-k" 'windmove-up
   "s-l" 'windmove-right))
(global-set-keys
 "s-o" 'delete-other-windows
 ;; "s-w" 'other-window
 "s-c" 'delete-window)


;;; Since `browse-url-default-browser' fails at seeing we can run xdg, force it.
(setq browse-url-browser-function
      (if (executable-find "xdg-open") 'browse-url-xdg-open 'browse-url-generic))
;;; If xdg-open is not found, set Emacs URL browser to the environment browser,
;;; or w3m if BROWSER is not set.
(setq browse-url-generic-program (or (executable-find (or (getenv "BROWSER") "")) (executable-find "w3m")))

;;; Default ispell dictionary. If not set, Emacs uses the current locale.
(setq ispell-dictionary "english")
(define-keys text-mode-map
  "C-<f6>" 'ispell-change-dictionary
  "<f6>" 'ispell-buffer)

;;; Long paragraphs. Useful for quick navigation with backward-paragraph and
;;; forward-paragraph. TODO: Useless?
;; (setq paragraph-start "
;; ")

;;; Show matching parenthesis
(show-paren-mode 1)
;;; By default, there’s a small delay before showing a matching parenthesis. Set
;;; it to 0 to deactivate.
(setq show-paren-delay 0)
(setq show-paren-when-point-inside-paren t)

;;; Electric Pairs to auto-complete () [] {} "" etc. It works on regions.
;; (electric-pair-mode)

;;; Spawn terminal shortcut: WM's binding is s+RET.
(global-set-key (kbd "C-x M-RET") 'spawn-terminal)

;;; Calendar ISO display.
(setq calendar-week-start-day 1)
(setq calendar-date-style 'iso)

;;; Compilation bindings and conveniences.
(setq compilation-ask-about-save nil)
(setq compilation-scroll-output 'first-error)
(with-eval-after-load 'compile
  ;; Making `compilation-directory' local only works with `recompile'
  ;; and if `compile' is never used. In such a scenario,
  ;; `compile-command' is not saved by `recompile' itself which adds a
  ;; lot of bookkeeping.
  ;; (make-variable-buffer-local 'compilation-directory)
  ;; (make-variable-buffer-local 'compile-history)
  (make-variable-buffer-local 'compile-command))
;;; Some commands ignore that compilation-mode is a "dumb" terminal and still display colors.
;;; Thus we render those colors.
(require 'ansi-color)
(defun compilation-colorize-buffer ()
  (when (eq major-mode 'compilation-mode)
    (ansi-color-apply-on-region compilation-filter-start (point-max))))
(add-hook 'compilation-filter-hook 'compilation-colorize-buffer)
(global-set-key (kbd "<f7>") 'previous-error)
(global-set-key (kbd "<f8>") 'next-error)
(define-keys prog-mode-map
  "C-<f6>" 'compile
  ;; Do not use `recompile' since we want to change de compilation folder to the current buffer.
  "<f6>" (lambda () (interactive) (compile compile-command)))

;;; Comint mode
(setq comint-prompt-read-only t)

(defadvice desktop-owner (after pry-from-cold-dead-hands activate)
  "Don't allow dead emacsen to own the desktop file."
  (when (not (emacs-process-p ad-return-value))
    (setq ad-return-value nil)))

;;; Desktop-mode
;;; Let Emacs auto-load/save sessions only when running the daemon.
;;; `server-running-p' is only useful once the daemon is started and cannot be
;;; used for initialization. We use `daemonp' instead.
;;; TODO: Desktop does not get saved when Emacs quits.  When does it get saved?
;;; TODO: `desktop-kill' should not query the user in `kill-emacs-hook'.
;;; TODO: Desktop mode does not save window registers properly.
;;; See https://groups.google.com/forum/#!topic/gnu.emacs.help/64aO_O43530
;;; and https://www.reddit.com/r/emacs/comments/4q38s1/save_register_between_sessions/?st=j419vc7r&sh=2617ffb4
;;; and http://debbugs.gnu.org/cgi/bugreport.cgi?bug=27422
;;; and https://stackoverflow.com/questions/5830494/windows-configuration-to-registers#5830928.
(when (daemonp)
  (setq history-length 250)
  (setq desktop-dirname (concat emacs-cache-folder "desktop"))
  (unless (file-directory-p desktop-dirname)
    (make-directory desktop-dirname t))
  (setq desktop-path (list desktop-dirname))
  (setq desktop-save t)
  ;; TODO: `compile-history' should be buffer local but that does not work.
  ;; http://user42.tuxfamily.org/compile-history-local/index.html
  ;; http://stackoverflow.com/questions/22995203/one-compile-command-per-buffer-not-directory
  ;; (add-to-list 'desktop-locals-to-save 'compile-history)
  (add-to-list 'desktop-locals-to-save 'compile-command)
  (add-to-list 'desktop-locals-to-save 'ispell-local-dictionary)
  (desktop-save-mode))

;;; GMP documentation
(with-eval-after-load "info-look"
  (let ((mode-value (assoc 'c-mode (assoc 'symbol info-lookup-alist))))
    (setcar (nthcdr 3 mode-value)
            (cons '("(gmp)Function Index" nil "^ -.* " "\\>")
                  (nth 3 mode-value)))))

;;; Buffer names.
(setq uniquify-buffer-name-style 'forward)

;;; Skeleton settings
;;; Do not expand abbrevs in skeletons.
(setq-default skeleton-further-elements '((abbrev-mode nil)))
(turn-on-skeleton-markers)
(global-set-keys
 "C->" 'skeleton-next-position
 "C-<" (lambda () (interactive) (skeleton-next-position t)))

;;; Disable prompt (but leave warning) on git symlink.
(setq vc-follow-symlinks t)

;;; Clipboard and primary selection.
;; (setq select-enable-clipboard t)
(setq select-enable-primary t)

;;; Move mouse away.
(mouse-avoidance-mode 'banish)
;;; That binding is not very useful and gets in the way of C-<mouse-1>.
(global-unset-key (kbd "C-<down-mouse-1>"))

;;; Scroll zooming.
(global-set-keys
 "C-<wheel-down>" 'text-scale-decrease
 "C-<mouse-5>" 'text-scale-decrease
 "C-<wheel-up>" 'text-scale-increase
 "C-<mouse-4>" 'text-scale-increase)
(setq text-scale-mode-step 1.1)

;;; Sort
(setq sort-fold-case t)

;;; Replace not-so-useful comment-dwim binding.
(global-set-key "\M-;" 'comment-line)

;;; Eldoc: Disable if too distracting.
;; (global-eldoc-mode 0)
;; (setq eldoc-idle-delay 0.1) ; Could be even more distracting.

;;; Replace `kill-buffer' binding by `kill-this-buffer'.
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;;; Ediff
;;; TODO: Ediff does not seem to auto-refine.  Bug?  Compare daemon and no-daemon.
(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally)

;;; Trash
(setq delete-by-moving-to-trash t)

;;; Display Time World
(setq
 zoneinfo-style-world-list
 '(("UTC" "-")
   ("Europe/Paris" "France Germany Sweden")
   ("Asia/Calcutta" "India")
   ("Indian/Mauritius" "Mauritius")
   ("Africa/Tunis" "Tunisia")
   ("Asia/Ho_Chi_Minh" "Vietnam")
   ("Australia/Melbourne" "Melbourne")
   ("Africa/Nairobi" "Uganda")))

;;; Tramp
(setq tramp-persistency-file-name (concat emacs-cache-folder "tramp")
      tramp-backup-directory-alist backup-directory-alist)

;;; Frame title
(setq frame-title-format (concat "%b" (unless (daemonp) " [serverless]")))

(provide 'main)
