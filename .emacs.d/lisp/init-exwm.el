;;; EXWM

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
;; TODO: How to prevent sending keys to window? ":" in Qutebrowser does not work. Issue with evil?
;; TODO: Spawn select programs in floating mode. (E.g. mpv, mupen64plus, mplayer, qemu, steam, .exe (wine).)

;;; System tray
;; TODO: Test and see if status bar is not a better solution.
;; (require 'exwm-systemtray)
;; (exwm-systemtray-enable)

(exwm-input-set-key (kbd "s-r") #'exwm-reset)
;;; Those cannot be set globally: if Emacs would be run in another WM, the "s-"
;;; prefix will conflict with the WM bindings.
(exwm-input-set-key (kbd "s-x") #'exwm-input-toggle-keyboard)
(exwm-input-set-key (kbd "s-h") #'windmove-left)
(exwm-input-set-key (kbd "s-j") #'windmove-down)
(exwm-input-set-key (kbd "s-k") #'windmove-up)
(exwm-input-set-key (kbd "s-l") #'windmove-right)
(exwm-input-set-key (kbd "s-c") #'kill-this-buffer)
(exwm-input-set-key (kbd "s-|") #'swap-windows)
(exwm-input-set-key (kbd "s-b") #'list-buffers)
(exwm-input-set-key (kbd "s-f") #'find-file)
(exwm-input-set-key (kbd "s-SPC") #'exwm-floating-toggle-floating)

(defun exwm-layout-toggle-fullscreen-or-delete-other-windows ()
  (interactive)
  (if (eq major-mode 'exwm-mode)
      (exwm-layout-toggle-fullscreen)
    ;; TODO: A toggle would be nice, so that pressing s-o again would restore the deleted windows.
    ;; Use register?
    (delete-other-windows)))
(exwm-input-set-key (kbd "s-o") #'exwm-layout-toggle-fullscreen-or-delete-other-windows)

(when (require 'functions)
  (exwm-input-set-key (kbd "s-\\") #'toggle-window-split))

(when (require 'helm-config nil t)
  (exwm-input-set-key (kbd "s-b") #'helm-mini)
  (exwm-input-set-key (kbd "s-f") #'helm-find-files)
  (exwm-input-set-key (kbd "s-g") #'helm-grep-git-or-ag)
  (exwm-input-set-key (kbd "s-G") #'helm-grep-git-all-or-ag))

;; TODO: Move `switch-to-last-buffer' to functions.el.
(defun switch-to-last-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))
(exwm-input-set-key (kbd "s-<tab>") #'switch-to-last-buffer)
(when (require 'evil nil t)
  (exwm-input-set-key (kbd "s-<tab>") #'evil-switch-to-windows-last-buffer)
  (exwm-input-set-key (kbd "C-6") #'evil-switch-to-windows-last-buffer))

;;; Emacs mode shortcuts.
;; TODO: Move `org-find-first-agenda' to init-org.
(exwm-input-set-key (kbd "s-t") #'org-find-first-agenda)
;; TODO: Move `eshell-or-new-session' to init-eshell.
(exwm-input-set-key (kbd "s-<return>") #'eshell-or-new-session)
(when (require 'magit nil t)
  (exwm-input-set-key (kbd "s-v") #'magit-status))
(when (require 'emms nil t)
  (exwm-input-set-key (kbd "s-a") #'emms-smart-browse)
  (exwm-input-set-key (kbd "s-A") #'emms))
(when (require 'mu4e nil t)
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
(let (mixer vol-up vol-down vol-toggle)
  (case system-type
    (gnu/linux
     (defun exwm-start-volume-down ()
       "Lower volume 5% with amixer"
       (interactive)
       (let ((cmd "amixer set Master 5%- >/dev/null"))
         (start-process-shell-command cmd nil cmd)))
     (defun exwm-start-volume-up ()
       "Raise volume 5% with amixer"
       (interactive)
       (let ((cmd "amixer set Master 5%+ >/dev/null"))
         (start-process-shell-command cmd nil cmd)))
     (defun exwm-start-volume-toggle ()
       "Toggle sound with amixer"
       (interactive)
       (let ((cmd "amixer set Master toggle >/dev/null"))
         (start-process-shell-command cmd nil cmd))))
    (t
     (defun exwm-start-volume-down ()
       "Lower volume 5% with mixer"
       (interactive)
       (let ((cmd "mixer vol -5 >/dev/null"))
         (start-process-shell-command cmd nil cmd)))
     (defun exwm-start-volume-up ()
       "Raise volume 5% with mixer"
       (interactive)
       (let ((cmd "mixer vol +5 >/dev/null"))
         (start-process-shell-command cmd nil cmd)))
     (defun exwm-start-volume-toggle ()
       "Toggle sound with mixer"
       (interactive)
       (let ((cmd "mixer vol ^ >/dev/null"))
         (start-process-shell-command cmd nil cmd))))))
(exwm-input-set-key (kbd "s-<kp-subtract>") #'exwm-start-volume-down)
(exwm-input-set-key (kbd "s-<kp-add>") #'exwm-start-volume-up)
(exwm-input-set-key (kbd "s-<kp-enter>") #'exwm-start-volume-toggle)

;; 's-&': Launch application. TODO: Rebind to 's-!'? s-r? (Need to rebind exwm-reset to s-R then.)
(exwm-input-set-key (kbd "s-&")
                    (lambda (command)
                      (interactive (list (read-shell-command "$ ")))
                      (start-process-shell-command command nil command)))

(exwm-enable) ; TODO: This should be left to Emacs' commandline arguments.

(provide 'init-exwm)
