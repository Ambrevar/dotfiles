;;; EXWM

;; TODO: Athena+Xaw3d confuses xcape when binding Caps-lock to both L_Ctrl
;; escape, in which case it will procude <C-escape> in Emacs. In practice, it
;; means that `C-` keys will works but `<escape>` will need a fast double tap on
;; Caps Lock.
;; See https://github.com/ch11ng/exwm/issues/285.

;; A system monitor within Emacs is only useful if not using an external tool,
;; as is the case when using another WM.
;; TODO: Would it be possible to have permanenet right-aligned display?
;; https://github.com/zk-phi/symon/issues/32
(when (require 'symon nil t)
  (symon-mode))

;;; Rename buffer to window title.
(defun exwm-rename-buffer-to-title () (exwm-workspace-rename-buffer exwm-title))
(add-hook 'exwm-update-title-hook 'exwm-rename-buffer-to-title)

(add-hook 'exwm-floating-setup-hook 'exwm-layout-hide-mode-line)
(add-hook 'exwm-floating-exit-hook 'exwm-layout-show-mode-line)

;;; Allow non-floating resizing with mouse.
(setq window-divider-default-bottom-width 2
      window-divider-default-right-width 2)
(window-divider-mode)

;; TODO: Add support for status bar (dzen2, xmobar, i3bar).
;; TODO: Resizing floating windows with mouse does not work on Ubuntu Trusty.
;; TODO: How to prevent sending keys to window? ":" is always passed to Qutebrowser. Issue with evil?
;; Maybe not. Just switch between char-mode and line-mode. Or check `exwm-input-line-mode-passthrough'.
;; https://emacs.stackexchange.com/questions/33326/how-do-i-cut-and-paste-effectively-between-applications-while-using-exwm
;; TODO: Spawn select programs in floating mode. (E.g. mpv, mupen64plus, mplayer, qemu, steam, .exe (wine).)

;;; System tray
;; TODO: Test and see if status bar is not a better solution.
;; (require 'exwm-systemtray)
;; (exwm-systemtray-enable)

;;; Those cannot be set globally: if Emacs would be run in another WM, the "s-"
;;; prefix will conflict with the WM bindings.
(exwm-input-set-key (kbd "s-R") #'exwm-reset)
(exwm-input-set-key (kbd "s-x") #'exwm-input-toggle-keyboard)
(exwm-input-set-key (kbd "s-h") #'windmove-left)
(exwm-input-set-key (kbd "s-j") #'windmove-down)
(exwm-input-set-key (kbd "s-k") #'windmove-up)
(exwm-input-set-key (kbd "s-l") #'windmove-right)
(exwm-input-set-key (kbd "s-C") #'kill-this-buffer)
(exwm-input-set-key (kbd "s-|") #'swap-windows)
(exwm-input-set-key (kbd "s-b") #'list-buffers)
(exwm-input-set-key (kbd "s-f") #'find-file)
(exwm-input-set-key (kbd "s-SPC") #'exwm-floating-toggle-floating)

(defun exwm-layout-toggle-fullscreen-or-single-window ()
  (interactive)
  (if (eq major-mode 'exwm-mode)
      ;; REVIEW:`exwm-layout-toggle-fullscreen' needs to be called interactively.
      ;; See https://github.com/ch11ng/exwm/issues/282.
      (call-interactively 'exwm-layout-toggle-fullscreen)
    (require 'functions)
    (toggle-single-window)))
(exwm-input-set-key (kbd "s-o") #'exwm-layout-toggle-fullscreen-or-single-window)

(require 'functions)
(exwm-input-set-key (kbd "s-\\") #'toggle-window-split)

(when (require 'helm-config nil t)
  (exwm-input-set-key (kbd "s-c") #'helm-resume)
  (exwm-input-set-key (kbd "s-b") #'helm-mini)
  (exwm-input-set-key (kbd "s-f") #'helm-find-files)
  (exwm-input-set-key (kbd "s-g") #'helm-grep-git-or-ag)
  (exwm-input-set-key (kbd "s-G") #'helm-grep-git-all-or-ag))

(require 'functions)
(exwm-input-set-key (kbd "s-<tab>") #'switch-to-last-buffer)
(when (require 'evil nil t)
  (exwm-input-set-key (kbd "s-<tab>") #'evil-switch-to-windows-last-buffer)
  (exwm-input-set-key (kbd "C-6") #'evil-switch-to-windows-last-buffer))

;;; Emacs mode shortcuts.
(exwm-input-set-key (kbd "s-t") #'org-find-first-agenda)
(exwm-input-set-key (kbd "s-<return>") #'eshell-or-new-session)
(when (fboundp 'magit-status)
  (exwm-input-set-key (kbd "s-v") #'magit-status))
(when (fboundp 'emms)
  (exwm-input-set-key (kbd "s-a") #'emms-smart-browse)
  (exwm-input-set-key (kbd "s-A") #'emms))
(when (delq nil (mapcar (lambda (path) (string-match "/mu4e/\\|/mu4e$" path)) load-path))
  (exwm-input-set-key (kbd "s-m") #'mu4e-headers-unread))

;;; External application shortcuts.
(defun exwm-start-browser () (interactive) (start-process-shell-command browse-url-generic-program nil browse-url-generic-program))
(exwm-input-set-key (kbd "s-w") #'exwm-start-browser)

(defvar exwm-lock-program "slock" "Shell command used to lock the screen.")
(defun exwm-start-lock () (interactive) (start-process-shell-command exwm-lock-program nil exwm-lock-program))
(exwm-input-set-key (kbd "s-z") #'exwm-start-lock)

(defun exwm-start-screenshot () (interactive) (start-process-shell-command "scrot" nil "scrot ~/temp/screen-%F-%T.png"))
(exwm-input-set-key (kbd "<print>") #'exwm-start-screenshot)

;; TODO: Check out the 'volume' package.
(defun exwm-volume (&optional up-or-down)
  (let ((controllers '(("amixer" "set Master" "5%-" "5%+" "toggle")
                       ("mixer" "vol" "-5" "+5" "^"))))
    (while (not (executable-find (caar controllers)))
      (setq controllers (cdr controllers)))
    (when controllers
      (setq controllers (car controllers))
      (start-process-shell-command
       "volume control" nil (format "%s %s %s >/dev/null"
                                    (car controllers)
                                    (cadr controllers)
                                    (nth (pcase up-or-down ('down  2) ('up  3) (_  4)) controllers))))))

(defun exwm-start-volume-down () (interactive) (exwm-volume 'down))
(defun exwm-start-volume-up () (interactive) (exwm-volume 'up))
(defun exwm-start-volume-toggle () (interactive) (exwm-volume))
(exwm-input-set-key (kbd "s-<kp-subtract>") #'exwm-start-volume-down)
(exwm-input-set-key (kbd "s-<kp-add>") #'exwm-start-volume-up)
(exwm-input-set-key (kbd "s-<kp-enter>") #'exwm-start-volume-toggle)

(defun exwm-start (command)
  (interactive (list (read-shell-command "$ ")))
  (start-process-shell-command command nil command))
(exwm-input-set-key (kbd "s-&") #'exwm-start)
(exwm-input-set-key (kbd "s-r") #'exwm-start)

(provide 'init-exwm)
