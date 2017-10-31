;;; helm-exwm

(defun foo ()
  (dolist (b (buffer-list))
    (let (victim count)
      (with-current-buffer b
        (when (string-match "duck" (buffer-name))
          (setq victim b)))
      (when victim
        (setq count (kill-buffer victim))
        (message "HELM killed %s" count)))))


;; TODO: Post on EXWM's wiki once all TODOs are fixed.
;; Publish on MELPA?  Maybe with generic code for EXWM buffers with column
;; containing the class name, and and emacs-buffers helm source too.

;; TODO: Change namespace to helm-exwm.

;; TODO: s-w s-w loses focus.
;; We should get
;;   helm: Error: Trying to run helm within a running helm session
;; but we don't.

;; TODO: Helm buffer does not die?
;; TODO: kill-persistent is not persistent.

;; TODO: Killing buffers message "Killed 0 buffer(s)".
;; See https://github.com/ch11ng/exwm/issues/322.

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

;; (defun exwm/helm-update ()
;; (message "EXWM/HELM"))

(defun exwm/helm-browser-buffers ()
  "Preconfigured `helm' to list browser buffers."
  (interactive)
  (helm :sources
        (helm-build-sync-source "exwm/helm browser buffers"
          :candidates
          (let ((bufs (delq nil (mapcar
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
            (message "HELM %s" bufs)
            bufs)
          :action '(("Switch to browser buffer(s)" . helm-buffer-switch-buffers)
                    ("Switch to browser buffer(s) in other window `C-c o'" . helm-buffer-switch-buffers-other-window)
                    ("Switch to browser buffer in other frame `C-c C-o'" . switch-to-buffer-other-frame)
                    ("Kill browser buffer(s)" . helm-kill-marked-buffers))
          ;; When follow-mode is on, the persistent-action allows for multiple candidate selection.
          ;; :persistent-action 'helm-buffers-list-persistent-action
          ;; :update 'exwm/helm-update
          :keymap exwm/helm-browser-map)
        :buffer "*exwm/helm browser*"))

(add-to-list 'helm-source-names-using-follow "exwm/helm browser buffers"))

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
        (when other-window (select-window (split-window-sensibly)))
        (start-process-shell-command browse-url-generic-program nil browse-url-generic-program)))))

(defun exwm-start-browser-other-window ()
  "Like `exwm-start-browser' but use other window if possible."
  (interactive)
  (exwm-start-browser t))
(exwm-input-set-key (kbd "s-w") #'exwm-start-browser)
(exwm-input-set-key (kbd "s-W") #'exwm-start-browser-other-window)

(provide 'package-helm-exwm)
