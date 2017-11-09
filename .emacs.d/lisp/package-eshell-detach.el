;;; eshell-detach.el

;; Instead of sending the command prompt to Eshell, send it to a bash shell run in a dtach session.
;; dtach allows the user to disconnect (quit Eshell or even Emacs) while the command keeps going.
;; Stderr and stdout can be both displayed on screen and redirected to a file thanks to the `tee' program.

;; dtach allows the commandline (that is, bash) to run in the background.
;; bash allows for:
;; - pausing the execution of the program;
;; - executing pipe lines (e.g. grep foo | sed ... | cut | wc).  dtach cannot do that alone.
;;
;; Bash is necessary here because if we want to run Eshell within dtach, we need to run Emacs in --batch mode.
;;   emacs --batch --eval '(progn (eshell) (insert "echo hello") (eshell-send-input))'
;; Issues: --batch sends to stderr.  How do we redirect the output to the real stdout/stderr?

(defvar eshell-detach-program "dtach"
  "The `dtach' program.")

(defvar eshell-detach-redraw-method nil
  "If nil, use the default value.
Value must be a string.
See dtach(1) for possible values.")

(defvar eshell-detach-shell "bash"
  "Shell to run the command in.
Should be bash-compatible.
The end command will be

  \"`eshell-detach-shell' -c { { <command>; } > >(tee stdout) } 2> >(tee stderr) | tee stdout+stderr\"")

;; TODO: Set the detach character?  No need when `C-c C-c` suffices.
(defvar eshell-detach-detach-character "^\\"
  "Charcter to press to detach dtach, i.e. leave the process run in the background.
The character syntax follows terminal notations, not Emacs.")

(defvar eshell-detach-detach-character-binding "C-\\"
  "The Emacs binding matching `eshell-detach-detach-character'.")

(defvar eshell-detach-socket-ext ".socket"
  "The file name extension for the socket fo `eshell-detach-program'.")

(defvar eshell-detach-stdout-ext ".stdout"
  "If non-nil and a string, stdout will also be saved to file named after the socket with this extension appened.
The 'tee' program is required.")

(defvar eshell-detach-stderr-ext ".stderr"
  "If non-nil and a string, stderr will also be saved to file named after the socket with this extension appened.
The 'tee' program is required.")

(defvar eshell-detach-stdout+stderr-ext ".stdout+stderr"
  "If non-nil and a string, stdout and stderr will also be saved to file named after the socket with this extension appened.
The 'tee' program is required.")

(defvar eshell-detach-directory (if server-socket-dir server-socket-dir temporary-file-directory)
  "The directory where to store the dtach socket and the logs.")

;; `eshell-named-command-hook' is not the way to go as it won't take pipelines.  What about
;; `eshell-rewrite-command-hook'?
(defun eshell-detach-rewrite-input (input)
  "Rewrite INPUT so that it is ready for detaching."
  ;; Since sockets get killed on termination, there won't be any leftover if
  ;; there is no log.  Thus it is cleaner to _not_ create a sub-directory.
  ;; `tee' creates log files even if nothing is output.  We cleanup on exit by
  ;; deleting 0-byte files.
  (let* (
         ;; TODO: temp-file should not exist for dtach to start?  That forces us
         ;; to use make-temp-file which is vulnerable to race condition.
         (socket (concat
                  (make-temp-name
                   (expand-file-name
                    (concat "dtach-"
                            (replace-regexp-in-string "[^A-Za-z0-9=-]" "_" input)
                            "-" (format-time-string "%F-%R:%S") "-")
                    eshell-detach-directory))
                  eshell-detach-socket-ext))
         (stdout (if eshell-detach-stdout-ext (concat socket eshell-detach-stdout-ext) nil))
         (stderr (if eshell-detach-stderr-ext (concat socket eshell-detach-stderr-ext) nil))
         (stdout+stderr (if eshell-detach-stdout+stderr-ext (concat socket eshell-detach-stdout+stderr-ext) nil))
         ;; The following test command was inspired by
         ;; https://stackoverflow.com/questions/21465297/tee-stdout-and-stderr-to-separate-files-while-retaining-them-on-their-respective
         ;; { { echo stdout; echo stderr >&2; } > >(tee stdout.txt ); } 2> >(tee stderr.txt ) | tee stdout+stderr.txt
         (commandline (format "{ { %s; }%s }%s %s; for i in %s %s %s; do [ ! -s \"$i\" ] && rm -- \"$i\"; done"
                              input
                              (if stdout (format " > >(tee %s );" stdout) "")
                              (if stderr (format " 2> >(tee %s )" stderr) "")
                              (if stdout+stderr (format " | tee %s" stdout+stderr) "")
                              (shell-quote-argument (or stdout ""))
                              (shell-quote-argument (or stderr ""))
                              (shell-quote-argument (or stdout+stderr "")))))
    (format "%s -c %s -z %s -c %s" eshell-detach-program socket eshell-detach-shell (shell-quote-argument commandline))))

(defun eshell-detach--list-sockets ()
  "List sockets of `eshell-detach-program'."
  (file-expand-wildcards (concat
                          (expand-file-name "dtach-"
                                            eshell-detach-directory)
                          "*" eshell-detach-socket-ext)))

(defun eshell-detach-attach ()
  "Attach to a running session of `eshell-detach-program'."
  (interactive)
  (let ((socket (completing-read "Attach to session: " (eshell-detach--list-sockets) nil t)))
    (when socket
      (when (or (eshell-interactive-process)
                (/= (point) eshell-last-output-end))
        (eshell-interrupt-process))
      (goto-char (point-max))
      ;; TODO: Redraw method?
      (insert eshell-detach-program " -a " (shell-quote-argument socket))
      (eshell-send-input))))

;;; This is almost an exact copy of `eshell-send-input'.
(defun eshell-detach-send-input (&optional use-region queue-p no-newline)
  "Send the input received to Eshell for parsing and processing.
After `eshell-last-output-end', sends all text from that marker to
point as input.  Before that marker, calls `eshell-get-old-input' to
retrieve old input, copies it to the end of the buffer, and sends it.

If USE-REGION is non-nil, the current region (between point and mark)
will be used as input.

If QUEUE-P is non-nil, input will be queued until the next prompt,
rather than sent to the currently active process.  If no process, the
input is processed immediately.

If NO-NEWLINE is non-nil, the input is sent without an implied final
newline."
  (interactive "P")
  ;; Note that the input string does not include its terminal newline.
  (let ((proc-running-p (and (eshell-interactive-process)
                             (not queue-p)))
        (inhibit-point-motion-hooks t)
        (inhibit-modification-hooks t))
    (unless (and proc-running-p
                 (not (eq (process-status
                           (eshell-interactive-process))
                          'run)))
      (if (or proc-running-p
              (>= (point) eshell-last-output-end))
          (goto-char (point-max))
        (let ((copy (eshell-get-old-input use-region)))
          (goto-char eshell-last-output-end)
          (insert-and-inherit copy)))
      (unless (or no-newline
                  (and eshell-send-direct-to-subprocesses
                       proc-running-p))
        (insert-before-markers-and-inherit ?\n))
      (if proc-running-p
          (progn
            (eshell-update-markers eshell-last-output-end)
            (if (or eshell-send-direct-to-subprocesses
                    (= eshell-last-input-start eshell-last-input-end))
                (unless no-newline
                  (process-send-string (eshell-interactive-process) "\n"))
              (process-send-region (eshell-interactive-process)
                                   eshell-last-input-start
                                   eshell-last-input-end)))
        (if (= eshell-last-output-end (point))
            (run-hooks 'eshell-post-command-hook)
          (let (input)
            (eshell-condition-case err
                (progn
                  (setq input (buffer-substring-no-properties
                               eshell-last-output-end (1- (point))))
                  (run-hook-with-args 'eshell-expand-input-functions
                                      eshell-last-output-end (1- (point)))
                  (let ((cmd
                         ;; TODO: This is the modification.  Report upstream the
                         ;; lack of flexibility.
                         ;; (eshell-parse-command-input
                         ;; eshell-last-output-end (1- (point)))))
                         (eshell-parse-command
                          (eshell-detach-rewrite-input input) nil t)))
                    (when cmd
                      (eshell-update-markers eshell-last-output-end)
                      (setq input (buffer-substring-no-properties
                                   eshell-last-input-start
                                   (1- eshell-last-input-end)))
                      (run-hooks 'eshell-input-filter-functions)
                      (and (catch 'eshell-terminal
                             (ignore
                              (if (eshell-invoke-directly cmd)
                                  (eval cmd)
                                (eshell-eval-command cmd input))))
                           (eshell-life-is-too-much)))))
              (quit
               (eshell-reset t)
               (run-hooks 'eshell-post-command-hook)
               (signal 'quit nil))
              (error
               (eshell-reset t)
               (eshell-interactive-print
                (concat (error-message-string err) "\n"))
               (run-hooks 'eshell-post-command-hook)
               (insert-and-inherit input)))))))))

;;; TODO: Remove bash / tee / dtach dependencies.
;;; See if `make-process' is the way to go: it supports stderr/stdout separation and stop/cont.
;;; Re-use `eshell-gather-process-output'?  Re-implement?

;; TODO: Pause/resume on Eshell.
;; Bash is one way:
;; (local-set-key (kbd "C-z") 'self-insert-command)
;; Pressing self-inserted "C-z RET" works.

;; Add C-c C-z binding.
;; TRY SENDING SIGCONT from Eshell.
;; TODO: If we run in current Eshell, can we send C-\? Try unbinding or self-insert.
;; (local-set-key (kbd "C-\\") 'self-insert-command)
;; Pressing self-inserted "C-\\ RET" works.

;;; From esh-proc.el
;;(defun eshell-stop-process ()
;;  "Send STOP signal to process."
;;  (interactive)
;;  (unless (eshell-process-interact 'stop-process)
;;    (eshell-kill-process-function nil "stopped")))

;;(defun eshell-continue-process ()
;;  "Send CONTINUE signal to process."
;;  (interactive)
;;  (unless (eshell-process-interact 'continue-process)
;;    ;; jww (1999-09-17): this signal is not dealt with yet.  For
;;    ;; example, `eshell-reset' will be called, and so will
;;    ;; `eshell-resume-eval'.
;;    (eshell-kill-process-function nil "continue")))

(provide 'package-eshell-detach)
