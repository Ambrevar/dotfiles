;;; helm-exwm

;; The following works, so `kill-buffer' on an EXWM window is fine.
(defun helm-exwm-kill-all ()
  (dolist (b (buffer-list))
    (with-current-buffer b
      (when (eq major-mode 'exwm-mode)
        (message "helm-exwm: Killing %s..." b)
        (kill-buffer b)
        (message "helm-exwm: Killed.")))))

;; TODO: Publish on MELPA.
;; TODO: Post on EXWM's wiki once on MELPA.

;; TODO: Maybe generic code for EXWM buffers with column
;; containing the class name.

;; TODO: Write a emacs-buffers helm source to filter out EXWM buffers from buffer list.

;; REVIEW: Helm buffer does not die?  Seems to be fixed.

;; TODO: kill-persistent is not persistent.

;; REVIEW: Killing buffers message "Killed 0 buffer(s)".
;; See https://github.com/ch11ng/exwm/issues/322.
;; A workaround would be to discard the result of kill-buffer and print the
;; count manually.

(defvar helm-exwm-map
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
  (with-helm-alive-p
    (helm-attrset 'kill-action '(helm-exwm-buffers-persistent-kill . never-split))
    (helm-execute-persistent-action 'kill-action)))
(put 'helm-exwm-buffer-run-kill-persistent 'helm-only t)

(defun helm-exwm-buffers-persistent-kill ()
  "Kill buffer without quitting helm."
  (interactive)
  (message "before")
  (kill-buffer (car (helm-marked-candidates)))
  (message "after"))

(defun helm-exwm-candidates (&optional class)
  "Return the list of EXWM buffers belonging to CLASS.

If CLASS is nil, then list all EXWM buffers."
  (let ((bufs (delq nil (mapcar
                         (lambda (buf)
                           (if (with-current-buffer buf
                                 (and (eq major-mode 'exwm-mode)
                                      (or (not class) (string= (downcase exwm-class-name) class))))
                               (buffer-name buf)
                             nil))
                         (buffer-list)))))
    (when (> (length bufs) 1)
      ;; Move first buffer (current) to last position.
      (setcdr (last bufs) (list (pop bufs))))
    bufs))

(defun helm-exwm-buffers (&optional class)
  "Preconfigured `helm' to list EXWM buffers belonging to CLASS.

If CLASS is nil, then list all EXWM buffers."
  (interactive)
  (helm :sources
        (helm-build-sync-source "EXWM buffers"
          :candidates (helm-exwm-candidates class)
          :action '(("Switch to browser buffer(s)" . helm-buffer-switch-buffers)
                    ("Switch to browser buffer(s) in other window `C-c o'" . helm-buffer-switch-buffers-other-window)
                    ("Switch to browser buffer in other frame `C-c C-o'" . switch-to-buffer-other-frame)
                    ("Kill browser buffer(s)" . helm-kill-marked-buffers))
          ;; When follow-mode is on, the persistent-action allows for multiple candidate selection.
          :persistent-action 'helm-buffers-list-persistent-action
          ;; :update 'helm-exwm-update
          :keymap helm-exwm-map)
        :buffer "*helm-exwm*"))

(defun helm-exwm-switch (class &optional program other-window)
  "Switch to some EXWM windows belonging to CLASS.

If current window is not showing CLASS, switch to the last open CLASS window.
If there is none, start PROGRAM.

If PROGRAM is nil, it defaults to CLASS.

With prefix argument or if OTHER-WINDOW is non-nil, open in other window."
  ;; If current window is not in `exwm-mode' we switch to it.  Therefore we must
  ;; also make sure that current window is not a Helm buffer, otherwise calling
  ;; this function will lose focus in Helm.
  (unless helm-alive-p
    (setq program (or program class)
          other-window (or other-window current-prefix-arg))
    (if (and (eq major-mode 'exwm-mode)
             (string= (downcase exwm-class-name) class))
        (if (fboundp 'helm-exwm-buffers)
            (helm-exwm-buffers class)
          (when other-window (other-window 1))
          (start-process-shell-command program nil program))
      (let ((last (buffer-list)))
        (while (and last
                    (not (with-current-buffer (car last)
                           (and (eq major-mode 'exwm-mode)
                                (string= (downcase exwm-class-name) class)))))
          (setq last (cdr last)))
        (if last
            (funcall (if other-window 'switch-to-buffer-other-window 'switch-to-buffer) (car last))
          (when other-window (select-window (split-window-sensibly)))
          (start-process-shell-command program nil program))))))

(defun helm-exwm-switch-browser ()
  "Switch to some `browse-url-generic-program' windows.

See `helm-exwm-start'."
  (interactive)
  (helm-exwm-switch (file-name-nondirectory browse-url-generic-program) browse-url-generic-program))

(defun helm-exwm-switch-browser-other-window ()
  "Switch to some `browse-url-generic-program' windows in other window.

See `helm-exwm-start'."
  (interactive)
  (helm-exwm-switch (file-name-nondirectory browse-url-generic-program) browse-url-generic-program t))

(exwm-input-set-key (kbd "s-w") #'helm-exwm-switch-browser)
(exwm-input-set-key (kbd "s-W") #'helm-exwm-switch-browser-other-window)

;; (add-to-list 'helm-source-names-using-follow "helm-exwm"))
;; (setq helm-source-names-using-follow nil)

(provide 'package-helm-exwm)
