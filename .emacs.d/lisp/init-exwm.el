;;; EXWM

;;; When stating the client from .xinitrc, `save-buffer-kill-terminal' will
;;; for-kill Emacs before it can run through `kill-emacs-hook'.
(global-set-key (kbd "C-x C-c") 'save-buffers-kill-emacs)

;;; REVIEW: Athena+Xaw3d confuses xcape when binding Caps-lock to both L_Ctrl
;;; escape, in which case it will procude <C-escape> in Emacs. In practice, it
;;; means that `C-` keys will works but `<escape>` will need a fast double tap
;;; on Caps Lock.
;;;
;;; See https://github.com/ch11ng/exwm/issues/285
;;; and https://gitlab.com/interception/linux/plugins/caps2esc/issues/2.

;;; TODO: Pressing "s-a" ('emms-smart-browse) loses the cursor.
;;; Sometimes waiting helps.  Calling emms-smart-browse manually does not trigger the issue.
;;; TODO: Spawn select programs in floating mode? (E.g. mpv, mupen64plus, mplayer, qemu, steam, .exe (wine).)

;;; TODO: Rendering issue with Qutebrowser
;;; Sometimes need to change window configuration
;;; https://github.com/ch11ng/exwm/issues/300
;;; Step to reproduce EXWM freeze

;;; REVIEW: helm-mini with follow-mode hangs when using EXWM.
;;; https://github.com/emacs-helm/helm/issues/1889

;;; Rename buffer to window title.
(defun exwm-rename-buffer-to-title () (exwm-workspace-rename-buffer exwm-title))
(add-hook 'exwm-update-title-hook 'exwm-rename-buffer-to-title)

(add-hook 'exwm-floating-setup-hook 'exwm-layout-hide-mode-line)
(add-hook 'exwm-floating-exit-hook 'exwm-layout-show-mode-line)

;;; Allow non-floating resizing with mouse.
(setq window-divider-default-bottom-width 2
      window-divider-default-right-width 2)
(window-divider-mode)

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
(exwm-input-set-key (kbd "s-b") #'list-buffers)
(exwm-input-set-key (kbd "s-f") #'find-file)

(when (require 'functions)
  (exwm-input-set-key (kbd "s-\\") #'toggle-window-split)
  (exwm-input-set-key (kbd "s-H") 'swap-windows-left)
  (exwm-input-set-key (kbd "s-J") 'swap-windows-below)
  (exwm-input-set-key (kbd "s-K") 'swap-windows-above)
  (exwm-input-set-key (kbd "s-L") 'swap-windows-right))

;; The following can only apply to EXWM buffers, else it could have unexpected effects.
(push ?\s-  exwm-input-prefix-keys)
(define-key exwm-mode-map (kbd "s-SPC") #'exwm-floating-toggle-floating)

(exwm-input-set-key (kbd "s-o") #'toggle-single-window)
(exwm-input-set-key (kbd "s-O") #'exwm-layout-toggle-fullscreen)

(with-eval-after-load 'helm
  ;; Need `with-eval-after-load' here since 'helm-map is not defined in 'helm-config.
  (define-keys helm-map
    "s-\\" 'helm-toggle-resplit-and-swap-windows)
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
(exwm-input-set-key (kbd "s-t") #'org-switch-agenda-file)
(exwm-input-set-key (kbd "s-T") #'org-switch-agenda-file-other-window)
(exwm-input-set-key (kbd "s-<return>") #'eshell-or-new-session)
(when (fboundp 'magit-status)
  (exwm-input-set-key (kbd "s-v") #'magit-status))
(when (fboundp 'emms-all)
  (exwm-input-set-key (kbd "s-a") #'emms-smart-browse)
  (exwm-input-set-key (kbd "S-s-<kp-enter>") #'emms-pause)
  (if (fboundp 'helm-emms)
      (exwm-input-set-key (kbd "s-A") #'helm-emms)
    (exwm-input-set-key (kbd "s-A") #'emms)))
(when (delq nil (mapcar (lambda (path) (string-match "/mu4e/\\|/mu4e$" path)) load-path))
  (exwm-input-set-key (kbd "s-m") #'mu4e-headers))



;;; External application shortcuts.
(when (require 'helm-exwm nil t)
  (add-to-list 'helm-source-names-using-follow "EXWM buffers")
  (setq helm-exwm-emacs-buffers-source (helm-exwm-build-emacs-buffers-source))
  (setq helm-exwm-source (helm-exwm-build-source))
  (setq helm-mini-default-sources `(helm-exwm-emacs-buffers-source
                                    helm-exwm-source
                                    helm-source-recentf
                                    ,(when (boundp 'helm-source-ls-git) 'helm-source-ls-git)
                                    helm-source-bookmarks
                                    helm-source-bookmark-set
                                    helm-source-buffer-not-found))
  (define-keys helm-exwm-map "M-d" 'helm-buffer-run-kill-persistent)
  (define-keys helm-exwm-map "M-o" 'helm-buffer-switch-other-window)
  ;; Web browser
  (exwm-input-set-key (kbd "s-w") #'helm-exwm-switch-browser)
  (exwm-input-set-key (kbd "s-W") #'helm-exwm-switch-browser-other-window))

;;; Lock screen
(defvar exwm-lock-program "slock" "Shell command used to lock the screen.")
(defun exwm-start-lock () (interactive) (start-process-shell-command exwm-lock-program nil exwm-lock-program))
(exwm-input-set-key (kbd "s-z") #'exwm-start-lock)

;;; Screenshot
(defun exwm-start-screenshot () (interactive) (start-process-shell-command "scrot" nil "scrot ~/temp/screen-%F-%T.png"))
(exwm-input-set-key (kbd "<print>") #'exwm-start-screenshot)

;;; Volume control
;;; TODO: Check out the 'volume' package.
(defun exwm-volume (&optional up-or-down)
  (let ((controllers '(("amixer" . ((control . "set Master") (down . "5%-") (up . "5%+") (toggle . "toggle")))
                       ("mixer" . ((control . "vol") (down . "-5") (up . "+5") (toggle . "^"))))))
    (while (not (executable-find (caar controllers)))
      (setq controllers (cdr controllers)))
    (when controllers
      (start-process-shell-command
       "volume control" nil (format "%s %s %s >/dev/null"
                                    (caar controllers)
                                    (alist-get 'control (cdar controllers))
                                    (alist-get up-or-down (cdar controllers) (alist-get 'toggle (cdar controllers) )))))))

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
            (lambda () (get-buffer "*Warnings*"))))))

;;; Some programs such as 'emacs' are better off being started in char-mode.
(defun exwm-start-in-char-mode ()
  (when (string= exwm-instance-name "emacs")
    (exwm-input-release-keyboard (exwm--buffer->id (window-buffer)))))
(add-hook 'exwm-manage-finish-hook 'exwm-start-in-char-mode)

(provide 'init-exwm)
