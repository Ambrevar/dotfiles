;;; helm-exwm

;; The following works, so `kill-buffer' on an EXWM window is fine.
(defun helm-exwm-kill-all ()
  (dolist (b (buffer-list))
    (with-current-buffer b
      (when (eq major-mode 'exwm-mode)
        (message "helm-exwm: Killing %s..." b)
        (kill-buffer b)
        (message "helm-exwm: Killed.")))))

;; TODO: Post on EXWM's wiki once all TODOs are fixed.
;; Publish on MELPA?  Maybe with generic code for EXWM buffers with column
;; containing the class name, and and emacs-buffers helm source too.

;; TODO: s-w s-w loses focus.
;; We don't get the expected error:
;;   "helm: Error: Trying to run helm within a running helm session"

;; TODO: Helm buffer does not die?

;; TODO: kill-persistent is not persistent.

;; REVIEW: Killing buffers message "Killed 0 buffer(s)".
;; See https://github.com/ch11ng/exwm/issues/322.
;; A workaround would be to discard the result of kill-buffer and print the
;; count manually.

(defvar helm-exwm-browser-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c o")     'helm-buffer-switch-other-window)
    (define-key map (kbd "C-c C-o")   'helm-buffer-switch-other-frame)
    (define-key map (kbd "M-D")       'helm-buffer-run-kill-buffers)
    ;; (define-key map (kbd "C-c d")     'helm-buffer-run-kill-persistent)
    (define-key map (kbd "C-c d")     'helm-exwm-buffer-run-kill-persistent)
    map)
  "Keymap for browser source in Helm.")

;;; This fails to be persistent, nothing is run after kill-buffer.
(defun helm-exwm-buffer-run-kill-persistent ()
  "Kill buffer without quitting helm."
  (interactive)
  (message "before")
  (kill-buffer (car (helm-marked-candidates)))
  (message "after"))
;; (put 'helm-exwm-buffer-run-kill-persistent 'helm-only t)

(defun helm-exwm-browser-buffers ()
  "Preconfigured `helm' to list browser buffers."
  (interactive)
  (helm :sources
        (helm-build-sync-source "helm-exwm browser buffers"
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
            bufs)
          :action '(("Switch to browser buffer(s)" . helm-buffer-switch-buffers)
                    ("Switch to browser buffer(s) in other window `C-c o'" . helm-buffer-switch-buffers-other-window)
                    ("Switch to browser buffer in other frame `C-c C-o'" . switch-to-buffer-other-frame)
                    ("Kill browser buffer(s)" . helm-kill-marked-buffers))
          ;; When follow-mode is on, the persistent-action allows for multiple candidate selection.
          :persistent-action 'helm-buffers-list-persistent-action
          ;; :update 'helm-exwm-update
          :keymap helm-exwm-browser-map)
        :buffer "*helm-exwm browser*"))

;; (add-to-list 'helm-source-names-using-follow "helm-exwm browser buffers"))
;; (setq helm-source-names-using-follow nil)

(defun exwm-start-browser (&optional other-window)
  "Fire-up the web browser as defined in `browse-url-generic-program'.
If current window is the web browser already, fire-up a new window.
If not, switch to the last open window.
If there is none, fire it up.

With prefix argument or if OTHER-WINDOW is non-nil, open in other window."
  (interactive "P")
  (if (and (eq major-mode 'exwm-mode)
           (string= (downcase exwm-class-name) (file-name-nondirectory browse-url-generic-program)))
      (if (fboundp 'helm-exwm-browser-buffers)
          (helm-exwm-browser-buffers)
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
