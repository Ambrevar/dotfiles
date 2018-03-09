;;; desktop.el patches

;;; Fix 28945.
;;; Replace
;;; (setq desktop-autosave-was-enabled
;;;   (memq 'desktop-auto-save-set-timer 'window-configuration-change-hook))
;;; with
;;; (setq desktop-autosave-was-enabled
;;;   (memq 'desktop-auto-save-set-timer (default-toplevel-value 'window-configuration-change-hook)))
(defun desktop-read (&optional dirname)
  "Read and process the desktop file in directory DIRNAME.
Look for a desktop file in DIRNAME, or if DIRNAME is omitted, look in
directories listed in `desktop-path'.  If a desktop file is found, it
is processed and `desktop-after-read-hook' is run.  If no desktop file
is found, clear the desktop and run `desktop-no-desktop-file-hook'.
This function is a no-op when Emacs is running in batch mode.
It returns t if a desktop file was loaded, nil otherwise."
  (interactive)
  (unless noninteractive
    (setq desktop-dirname
          (file-name-as-directory
           (expand-file-name
            (or
             ;; If DIRNAME is specified, use it.
             (and (< 0 (length dirname)) dirname)
             ;; Otherwise search desktop file in desktop-path.
             (let ((dirs desktop-path))
               (while (and dirs
                           (not (file-exists-p
                                 (desktop-full-file-name (car dirs)))))
                 (setq dirs (cdr dirs)))
               (and dirs (car dirs)))
             ;; If not found and `desktop-path' is non-nil, use its first element.
             (and desktop-path (car desktop-path))
             ;; Default: .emacs.d.
             user-emacs-directory))))
    (if (file-exists-p (desktop-full-file-name))
        ;; Desktop file found, but is it already in use?
        (let ((desktop-first-buffer nil)
              (desktop-buffer-ok-count 0)
              (desktop-buffer-fail-count 0)
              (owner (desktop-owner))
              ;; Avoid desktop saving during evaluation of desktop buffer.
              (desktop-save nil)
              (desktop-autosave-was-enabled))
          (if (and owner
                   (memq desktop-load-locked-desktop '(nil ask))
                   (or (null desktop-load-locked-desktop)
                       (and (daemonp) (= (length (visible-frame-list)) 1)) ; PATCH
                       (not (y-or-n-p (format "Warning: desktop file appears to be in use by PID %s.\n\
Using it may cause conflicts.  Use it anyway? " owner)))))
              (let ((default-directory desktop-dirname))
                (setq desktop-dirname nil)
                (run-hooks 'desktop-not-loaded-hook)
                (unless desktop-dirname
                  (message "Desktop file in use; not loaded.")))
            (desktop-lazy-abort)
            ;; Temporarily disable the autosave that will leave it
            ;; disabled when loading the desktop fails with errors,
            ;; thus not overwriting the desktop with broken contents.
            (setq desktop-autosave-was-enabled
                  (memq 'desktop-auto-save-set-timer
                        ;; Use the toplevel value of the hook, in case some
                        ;; feature makes window-configuration-change-hook
                        ;; buffer-local, and puts there stuff which
                        ;; doesn't include our timer.
                        (default-toplevel-value
                          'window-configuration-change-hook)))
            (desktop-auto-save-disable)
            ;; Evaluate desktop buffer and remember when it was modified.
            (setq desktop-file-modtime (nth 5 (file-attributes (desktop-full-file-name))))
            (load (desktop-full-file-name) t t t)
            ;; If it wasn't already, mark it as in-use, to bother other
            ;; desktop instances.
            (unless (eq (emacs-pid) owner)
              (condition-case nil
                  (desktop-claim-lock)
                (file-error (message "Couldn't record use of desktop file")
                            (sit-for 1))))

            (unless (desktop-restoring-frameset-p)
              ;; `desktop-create-buffer' puts buffers at end of the buffer list.
              ;; We want buffers existing prior to evaluating the desktop (and
              ;; not reused) to be placed at the end of the buffer list, so we
              ;; move them here.
              (mapc 'bury-buffer
                    (nreverse (cdr (memq desktop-first-buffer (nreverse (buffer-list))))))
              (switch-to-buffer (car (buffer-list))))
            (run-hooks 'desktop-delay-hook)
            (setq desktop-delay-hook nil)
            (desktop-restore-frameset)
            (run-hooks 'desktop-after-read-hook)
            (message "Desktop: %s%d buffer%s restored%s%s."
                     (if desktop-saved-frameset
                         (let ((fn (length (frameset-states desktop-saved-frameset))))
                           (format "%d frame%s, "
                                   fn (if (= fn 1) "" "s")))
                       "")
                     desktop-buffer-ok-count
                     (if (= 1 desktop-buffer-ok-count) "" "s")
                     (if (< 0 desktop-buffer-fail-count)
                         (format ", %d failed to restore" desktop-buffer-fail-count)
                       "")
                     (if desktop-buffer-args-list
                         (format ", %d to restore lazily"
                                 (length desktop-buffer-args-list))
                       ""))
            (unless (desktop-restoring-frameset-p)
              ;; Bury the *Messages* buffer to not reshow it when burying
              ;; the buffer we switched to above.
              (when (buffer-live-p (get-buffer "*Messages*"))
                (bury-buffer "*Messages*"))
              ;; Clear all windows' previous and next buffers, these have
              ;; been corrupted by the `switch-to-buffer' calls in
              ;; `desktop-restore-file-buffer' (bug#11556).  This is a
              ;; brute force fix and should be replaced by a more subtle
              ;; strategy eventually.
              (walk-window-tree (lambda (window)
                                  (set-window-prev-buffers window nil)
                                  (set-window-next-buffers window nil))))
            (setq desktop-saved-frameset nil)
            (if desktop-autosave-was-enabled (desktop-auto-save-enable))
            t))
      ;; No desktop file found.
      (let ((default-directory desktop-dirname))
        (run-hooks 'desktop-no-desktop-file-hook))
      (message "No desktop file.")
      nil)))

;;; Fix 30421
;;; Return nil if the PID found is not owned by a process named Emacs.
;;; This assumes that no other name would be using the desktop file.
;;; Upstream decided not to do this because of possible clashes when emacs
;;; process is running remotely.  Start desktop-mode in
;;; `server-after-make-frame-hook' instead (requires Emacs>=27).
(defun desktop-owner (&optional dirname)
  "Return the PID of the Emacs process that owns the desktop file in DIRNAME.
Return nil if no desktop file found or no Emacs process is using it.
DIRNAME omitted or nil means use `desktop-dirname'."
  (let (owner
        (file (desktop-full-lock-name dirname)))
    (and (file-exists-p file)
         (ignore-errors
           (with-temp-buffer
             (insert-file-contents-literally file)
             (goto-char (point-min))
             (setq owner (read (current-buffer)))
             (integerp owner)
             (process-attributes owner)
             (string= "emacs" (alist-get 'comm (process-attributes owner)))))
         owner)))
