;;; EXWM

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
;;; TODO: Separate EXWM buffers and Emacs buffers in `helm-mini'?

;;; TODO: Rendering issue with Qutebrowser
;;; Sometimes need to change window configuration
;;; https://github.com/ch11ng/exwm/issues/300
;;; Step to reproduce EXWM freeze

;;; TODO: helm-mini with follow-mode hangs when using EXWM.
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
(exwm-input-set-key (kbd "s-|") #'swap-windows)
(exwm-input-set-key (kbd "s-b") #'list-buffers)
(exwm-input-set-key (kbd "s-f") #'find-file)
(exwm-input-set-key (kbd "s-SPC") #'exwm-floating-safe-toggle-floating)

;;; REVIEW: https://github.com/ch11ng/exwm/issues/314
(defun exwm-floating-safe-toggle-floating ()
  (interactive)
  (unless (minibufferp)
    (exwm-floating-toggle-floating)))

(exwm-input-set-key (kbd "s-o") #'toggle-single-window)
(exwm-input-set-key (kbd "s-O") #'exwm-layout-toggle-fullscreen)

(require 'functions)
(exwm-input-set-key (kbd "s-\\") #'toggle-window-split)

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
  (exwm-input-set-key (kbd "s-A") #'emms))
(when (delq nil (mapcar (lambda (path) (string-match "/mu4e/\\|/mu4e$" path)) load-path))
  (exwm-input-set-key (kbd "s-m") #'mu4e-headers))



;;; External application shortcuts.

;;; Web browser
(with-eval-after-load 'helm
  ;; TODO: Post on EXWM's wiki once all TODOs are fixed.
  ;; Publish on MELPA?  Maybe with generic code for EXWM buffers with column
  ;; containing the class name, and and emacs-buffers helm source too.
  ;; REVIEW: When follow-mode is on, multiselection is broken.
  ;; TODO: s-w s-w loses focus.
  ;; TODO: kill-persistent is not persistent.
  (defvar exwm/helm-browser-map
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map helm-map)
      (define-key map (kbd "C-c o")     'helm-buffer-switch-other-window)
      (define-key map (kbd "C-c C-o")   'helm-buffer-switch-other-frame)
      (define-key map (kbd "M-D")       'helm-buffer-run-kill-buffers)
      (define-key map (kbd "C-c d")     'helm-buffer-run-kill-persistent)
      ;; (define-key map (kbd "C-c d")     'exwm/helm-browsers-run-kill-persistent)
      map)
    "Keymap for browser source in Helm.")

  (defun exwm/helm-update ()
    (message "EXWM/HELM"))

  (defun exwm/helm-browser-buffers ()
    "Preconfigured `helm' to list browser buffers."
    (interactive)
    (helm :sources
          (helm-build-sync-source (concat (or exwm-class-name (file-name-nondirectory browse-url-generic-program)) " buffers")
            :candidates
            (let (
                  (bufs (delq nil (mapcar
                                   (lambda (buf)
                                     (if (with-current-buffer buf
                                           (and (eq major-mode 'exwm-mode)
                                                (string= (downcase exwm-class-name) (file-name-nondirectory browse-url-generic-program))))
                                         (buffer-name buf)
                                       nil))
                                   (buffer-list)))))
              (when bufs
                ;; Move first buffer (current) to last position.
                (setcdr (last bufs) (list (pop bufs))))
              bufs)
            :action '(("Switch to browser buffer(s)" . helm-buffer-switch-buffers)
                      ("Switch to browser buffer(s) in other window `C-c o'" . helm-buffer-switch-buffers-other-window)
                      ("Switch to browser buffer in other frame `C-c C-o'" . switch-to-buffer-other-frame)
                      ("Kill browser buffer(s)" . helm-kill-marked-buffers))
            ;; When follow-mode is on, the persistent-action allows for multiple candidate selection.
            :persistent-action 'helm-buffers-list-persistent-action
            :update 'exwm/helm-update
            :keymap exwm/helm-browser-map)
          :buffer "*exwm/helm browser*"))

  ;; REVIEW: Does this work?
  (add-to-list 'helm-source-names-using-follow "exwm/helm browser"))

(defun exwm-start-browser (&optional other-window)
  "Fire-up the web browser as defined in `browse-url-generic-program'.
If current window is the web browser already, fire-up a new window.
If not, switch to the last open window.
If there is none, fire it up.

With prefix argument or if OTHER-WINDOW is non-nil, open in other window."
  (interactive "P")
  (if (and (eq major-mode 'exwm-mode)
           (string= (downcase exwm-class-name) (file-name-nondirectory browse-url-generic-program)))
      (if (fboundp 'exwm/helm-browser-buffers)
          (exwm/helm-browser-buffers)
        (when other-window (other-window 1))
        (start-process-shell-command browse-url-generic-program nil browse-url-generic-program))
    (let ((last (buffer-list)))
      (while (and last
                  (not (with-current-buffer (car last)
                         (and (eq major-mode 'exwm-mode)
                              (string= (downcase exwm-class-name) (file-name-nondirectory browse-url-generic-program))))))
        (setq last (cdr last)))
      (if last
          (funcall (if other-window 'switch-to-buffer-other-window 'switch-to-buffer) (car last))
        (select-window (split-window))
        (start-process-shell-command browse-url-generic-program nil browse-url-generic-program)))))
(defun exwm-start-browser-other-window ()
  "Like `exwm-start-browser' but use other window if possible."
  (interactive)
  (exwm-start-browser t))
(exwm-input-set-key (kbd "s-w") #'exwm-start-browser)
(exwm-input-set-key (kbd "s-W") #'exwm-start-browser-other-window)

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
