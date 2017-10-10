;;; EXWM

;;; REVIEW: Athena+Xaw3d confuses xcape when binding Caps-lock to both L_Ctrl
;;; escape, in which case it will procude <C-escape> in Emacs. In practice, it
;;; means that `C-` keys will works but `<escape>` will need a fast double tap
;;; on Caps Lock.
;;;
;;; See https://github.com/ch11ng/exwm/issues/285
;;; and https://gitlab.com/interception/linux/plugins/caps2esc/issues/2.

;;; Rename buffer to window title.
(defun exwm-rename-buffer-to-title () (exwm-workspace-rename-buffer exwm-title))
(add-hook 'exwm-update-title-hook 'exwm-rename-buffer-to-title)

(add-hook 'exwm-floating-setup-hook 'exwm-layout-hide-mode-line)
(add-hook 'exwm-floating-exit-hook 'exwm-layout-show-mode-line)

;;; Allow non-floating resizing with mouse.
(setq window-divider-default-bottom-width 2
      window-divider-default-right-width 2)
(window-divider-mode)

;;; REVIEW: Resizing floating windows with mouse does not work on Ubuntu Trusty.
;;; See https://github.com/ch11ng/exwm/issues/283.
;; exwm-input--move-keysym 1
;; exwm-input--move-mask 64
;; exwm-input--resize-keysym 3
;; exwm-input--resize-mask 64
;;; TODO: Spawn select programs in floating mode. (E.g. mpv, mupen64plus, mplayer, qemu, steam, .exe (wine).)

;;; System tray
(require 'exwm-systemtray)
(exwm-systemtray-enable)
(setq exwm-systemtray-height 16)

;;; Those cannot be set globally: if Emacs would be run in another WM, the "s-"
;;; prefix will conflict with the WM bindings.
(exwm-input-set-key (kbd "s-R") #'exwm-reset)
(exwm-input-set-key (kbd "s-x") #'exwm-input-toggle-keyboard)
(exwm-input-set-key (kbd "s-h") #'windmove-left)
(exwm-input-set-key (kbd "s-j") #'windmove-down)
(exwm-input-set-key (kbd "s-k") #'windmove-up)
(exwm-input-set-key (kbd "s-l") #'windmove-right)
(exwm-input-set-key (kbd "s-D") #'kill-this-buffer)
(exwm-input-set-key (kbd "s-|") #'swap-windows)
(exwm-input-set-key (kbd "s-b") #'list-buffers)
(exwm-input-set-key (kbd "s-f") #'find-file)
(exwm-input-set-key (kbd "s-SPC") #'exwm-floating-toggle-floating)

(exwm-input-set-key (kbd "s-o") #'toggle-single-window)
(exwm-input-set-key (kbd "s-O") #'exwm-layout-toggle-fullscreen)

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
(when (fboundp 'emms-all)
  (exwm-input-set-key (kbd "s-a") #'emms-smart-browse)
  (exwm-input-set-key (kbd "s-A") #'emms))
(when (delq nil (mapcar (lambda (path) (string-match "/mu4e/\\|/mu4e$" path)) load-path))
  (exwm-input-set-key (kbd "s-m") #'mu4e-headers))

;;; External application shortcuts.
(defun exwm-start-browser ()
  "Fire-up the web browser as defined in `browse-url-generic-program'.
If current window is the web browser already, fire-up a new window.
If not, switch to the last open window.
If there is none, fire it up."
  (interactive)
  (if (and (eq major-mode 'exwm-mode)
           (string-match
            ;; Only match against the end as some window names are hard to predict, e.g. "Mozilla Firefox".
            (format "%s$" (regexp-quote (file-name-nondirectory browse-url-generic-program)))
            (downcase (buffer-name (current-buffer)))))
      (start-process-shell-command browse-url-generic-program nil browse-url-generic-program)
    (let ((last (buffer-list)))
      (while (and last
                  (not (with-current-buffer (car last)
                         (and (eq major-mode 'exwm-mode)
                              (string-match
                               (format "%s$" (regexp-quote (file-name-nondirectory browse-url-generic-program)))
                               (downcase (buffer-name (current-buffer))))))))
        (setq last (cdr last)))
      (if last
          (switch-to-buffer (car last))
        (start-process-shell-command browse-url-generic-program nil browse-url-generic-program)))))
(exwm-input-set-key (kbd "s-w") #'exwm-start-browser)

(defvar exwm-lock-program "slock" "Shell command used to lock the screen.")
(defun exwm-start-lock () (interactive) (start-process-shell-command exwm-lock-program nil exwm-lock-program))
(exwm-input-set-key (kbd "s-z") #'exwm-start-lock)

(defun exwm-start-screenshot () (interactive) (start-process-shell-command "scrot" nil "scrot ~/temp/screen-%F-%T.png"))
(exwm-input-set-key (kbd "<print>") #'exwm-start-screenshot)

;;; TODO: Check out the 'volume' package.
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

;;; Check for start-up errors. See ~/.profile.
(let ((error-logs (directory-files "~" t "errors.*log$")))
  (when error-logs
    (warn "Error during system startup.  See %s." (mapconcat 'identity error-logs ", "))
    (when (daemonp)
      ;; Non-daemon Emacs already brings up the *Warning* buffer.
      (setq initial-buffer-choice
            (lambda ()
              (let ((b (get-buffer "*Warnings*")))
                (message "STARTUP: %s" b)
                b))))))

(provide 'init-exwm)
