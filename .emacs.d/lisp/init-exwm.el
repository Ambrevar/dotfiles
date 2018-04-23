;;; EXWM

;;; When stating the client from .xinitrc, `save-buffer-kill-terminal' will
;;; force-kill Emacs before it can run through `kill-emacs-hook'.
(global-set-key (kbd "C-x C-c") 'save-buffers-kill-emacs)

;;; REVIEW: Athena+Xaw3d confuses xcape when binding Caps-lock to both L_Ctrl
;;; escape, in which case it will procude <C-escape> in Emacs. In practice, it
;;; means that `C-` keys will works but `<escape>` will need a fast double tap
;;; on Caps Lock.
;;;
;;; See https://github.com/ch11ng/exwm/issues/285
;;; and https://gitlab.com/interception/linux/plugins/caps2esc/issues/2.

;;; REVIEW: Pressing "s-a" ('emms-smart-browse) loses the cursor.
;;; Sometimes waiting helps.  Calling emms-smart-browse manually does not trigger the issue.
;;; https://github.com/ch11ng/exwm/issues/366

;;; REVIEW: helm-mini with follow-mode hangs when using EXWM.
;;; https://github.com/emacs-helm/helm/issues/1889

;;; Rename buffer to window title.
(defun ambrevar/exwm-rename-buffer-to-title () (exwm-workspace-rename-buffer exwm-title))
(add-hook 'exwm-update-title-hook 'ambrevar/exwm-rename-buffer-to-title)

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
  (exwm-input-set-key (kbd "s-\\") 'ambrevar/toggle-window-split)
  (exwm-input-set-key (kbd "s-H") 'ambrevar/swap-windows-left)
  (exwm-input-set-key (kbd "s-J") 'ambrevar/swap-windows-below)
  (exwm-input-set-key (kbd "s-K") 'ambrevar/swap-windows-above)
  (exwm-input-set-key (kbd "s-L") 'ambrevar/swap-windows-right))

;; The following can only apply to EXWM buffers, else it could have unexpected effects.
(push ?\s-  exwm-input-prefix-keys)
(define-key exwm-mode-map (kbd "s-SPC") #'exwm-floating-toggle-floating)

(exwm-input-set-key (kbd "s-i") #'follow-delete-other-windows-and-split)
(exwm-input-set-key (kbd "s-o") #'ambrevar/toggle-single-window)
(exwm-input-set-key (kbd "s-O") #'exwm-layout-toggle-fullscreen)

(with-eval-after-load 'helm
  ;; Need `with-eval-after-load' here since 'helm-map is not defined in 'helm-config.
  (ambrevar/define-keys helm-map
                        "s-\\" 'helm-toggle-resplit-and-swap-windows)
  (exwm-input-set-key (kbd "s-c") #'helm-resume)
  (exwm-input-set-key (kbd "s-b") #'helm-mini)
  (exwm-input-set-key (kbd "s-f") #'helm-find-files)
  (exwm-input-set-key (kbd "s-F") #'helm-locate)
  (when (fboundp 'ambrevar/helm-locate-meta)
    (exwm-input-set-key (kbd "s-F") #'ambrevar/helm-locate-meta))
  (exwm-input-set-key (kbd "s-g") 'ambrevar/helm-grep-git-or-ag)
  (exwm-input-set-key (kbd "s-G") 'ambrevar/helm-grep-git-all-or-ag))

(require 'functions)
(exwm-input-set-key (kbd "s-<tab>") #'ambrevar/switch-to-last-buffer)
(when (require 'evil nil t)
  (exwm-input-set-key (kbd "s-<tab>") #'evil-switch-to-windows-last-buffer)
  (exwm-input-set-key (kbd "C-6") #'evil-switch-to-windows-last-buffer))

;;; Emacs mode shortcuts.
(exwm-input-set-key (kbd "s-t") #'ambrevar/org-switch-agenda-file)
(exwm-input-set-key (kbd "s-T") #'ambrevar/org-switch-agenda-file-other-window)
(exwm-input-set-key (kbd "s-<return>") #'ambrevar/eshell-or-new-session)
(when (fboundp 'magit-status)
  (exwm-input-set-key (kbd "s-v") #'magit-status))
(when (fboundp 'emms-all)
  (exwm-input-set-key (kbd "s-a") #'emms-smart-browse)
  (exwm-input-set-key (kbd "S-s-<kp-enter>") #'emms-pause)
  (if (fboundp 'helm-emms)
      (exwm-input-set-key (kbd "s-A") #'helm-emms)
    (exwm-input-set-key (kbd "s-A") #'emms)))
(when (or (fboundp 'mu4e)
          (delq nil (mapcar (lambda (path) (string-match "/mu4e/\\|/mu4e$" path)) load-path)))
  (exwm-input-set-key (kbd "s-m") #'ambrevar/mu4e-headers))
(exwm-input-set-key (kbd "s-n") #'ambrevar/elfeed-switch-back) ; "n" for "news"
(exwm-input-set-key (kbd "s-e") #'ambrevar/eww-switch-back)
(exwm-input-set-key (kbd "s-E") #'eww)

(when (fboundp 'helm-pass)
  (defun ambrevar/helm-pass-for-page ()
    "Default prompt to current exwm-title"
    (interactive)
    (require 'helm-pass)
    (helm :sources 'helm-pass-source-pass
          :input (cond
                  ((derived-mode-p 'eww-mode)
                   (let* ((url (replace-regexp-in-string ".*//\\([^/]*\\).*" "\\1" (eww-current-url)))
                          (domain (split-string url "\\.")))
                     (concat (nth (- (length domain) 2) domain) "." (nth (1- (length domain)) domain))))
                  ((and (derived-mode-p 'exwm-mode) exwm-title)
                   (let* ((url (car (last (split-string exwm-title " "))))
                          (domain (split-string url "\\.")))
                     (concat (nth (- (length domain) 2) domain) "." (nth (1- (length domain)) domain)))))
          :buffer "*helm-pass*"))
  (exwm-input-set-key (kbd "s-p") #'ambrevar/helm-pass-for-page))

;;; External application shortcuts.
(defun ambrevar/exwm-start (command)
  (interactive (list (read-shell-command "$ ")))
  (start-process-shell-command command nil command))
(exwm-input-set-key (kbd "s-&") #'ambrevar/exwm-start)
(exwm-input-set-key (kbd "s-r") #'ambrevar/exwm-start)

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
  (ambrevar/define-keys
   helm-exwm-map
   "M-d" 'helm-buffer-run-kill-persistent
   "S-<return>" 'helm-buffer-switch-other-window)
  ;; Launcher
  (exwm-input-set-key (kbd "s-r") 'helm-run-external-command)
  ;; Web browser
  (exwm-input-set-key (kbd "s-w") #'helm-exwm-switch-browser)
  (exwm-input-set-key (kbd "s-W") #'helm-exwm-switch-browser-other-window))

;;; Lock screen
(defun ambrevar/exwm-start-lock () (interactive) (start-process "slock" nil "slock"))
(exwm-input-set-key (kbd "s-z") #'ambrevar/exwm-start-lock)

;;; Screenshot
(defun ambrevar/exwm-start-screenshot () (interactive) (start-process-shell-command "scrot" nil "scrot ~/temp/screen-%F-%T.png"))
(exwm-input-set-key (kbd "<print>") #'ambrevar/exwm-start-screenshot)

;;; Volume control
(when (require 'pulseaudio-control nil t)
  (exwm-input-set-key (kbd "s-<kp-subtract>") #'pulseaudio-control-decrease-volume)
  (exwm-input-set-key (kbd "s-<kp-add>") #'pulseaudio-control-increase-volume)
  (exwm-input-set-key (kbd "s-<kp-enter>") #'pulseaudio-control-toggle-current-sink-mute)
  (exwm-input-set-key (kbd "s--") #'pulseaudio-control-decrease-volume)
  (exwm-input-set-key (kbd "s-=") #'pulseaudio-control-increase-volume)
  (exwm-input-set-key (kbd "s-0") #'pulseaudio-control-toggle-current-sink-mute))

;;; Check for start-up errors. See ~/.profile.
(let ((error-logs (directory-files "~" t "errors.*log$")))
  (when error-logs
    (warn "Error during system startup.  See %s." (mapconcat 'identity error-logs ", "))
    (when (daemonp)
      ;; Non-daemon Emacs already brings up the *Warning* buffer.
      (setq initial-buffer-choice
            (lambda () (get-buffer "*Warnings*"))))))

;;; Some programs such as 'emacs' are better off being started in char-mode.
(defun ambrevar/exwm-start-in-char-mode ()
  (when (string-prefix-p "emacs" exwm-instance-name)
    (exwm-input-release-keyboard (exwm--buffer->id (window-buffer)))))
(add-hook 'exwm-manage-finish-hook 'ambrevar/exwm-start-in-char-mode)

(provide 'init-exwm)
