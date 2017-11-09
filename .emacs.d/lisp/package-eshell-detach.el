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

;; TODO: Add C-c C-z binding for pausing.
;; TODO: Pause/resume on Eshell.
;; Bash is one way:
;; (local-set-key (kbd "C-z") 'self-insert-command)
;; Pressing self-inserted "C-z RET" works.
;; That only works for interactive shells, not with `bash -c'.

;; TODO: If we run in current Eshell, can we send C-\? Try unbinding or self-insert.
;;; esh-proc.el has `eshell-stop-process' but that's not seem to work.  Maybe it does not propagate properly.

;; Graph:
;; dtach (client)
;; - dtach (daemon)
;;   - bash (pipe of bash running process and tee of stdout+stderr)
;;     - bash (process)
;;       - bash (tee of stdout)
;;       - bash (tee of stderr)
;; We want to send a signal to "bash (process)".
;; We cannot predict how many process the command will start and we need top send signals to all of them.
;; TODO: Refactor.
;; TODO: Order by deepest child first so that we kill in order.
(defun eshell-detach-graph ()
  "Return the list of recursive children of dtach except the dtach daemon."
  (let (dtach-daemon
        dtach-client
        bash-root
        bash-middle
        bash-child1
        bash-child2
        bash-command-process-list
        result)
    (let* ((pids (list-system-processes))
           (ppids (mapcar (lambda (p) (cons (alist-get 'ppid (process-attributes p)) p)) pids)))
      (setq dtach-client (process-id (eshell-interactive-process)))
      (message "dtach client: %S" (alist-get 'args (process-attributes dtach-client))) ; debug only

      ;; Get dtach daemon.
      (setq dtach-daemon (alist-get dtach-client ppids))
      (when (null dtach-daemon)
        ;; If we are attaching to a socket, the client daemon is forket and thus
        ;; it is not a child of the client.
        ;; WARNING: Brittle code ahead.
        (let ((pids pids)) ; Save `pids'.
          (while (and pids (null dtach-daemon))
            ;; TODO: Use if-let to save process-attributes.
            (when (and (/= (car pids) dtach-client)
                       (string= (alist-get 'comm (process-attributes (car pids))) "dtach")
                       ;; Socket is the third substring.
                       (string=
                        (nth 2 (split-string (alist-get 'args (process-attributes (car pids)))))
                        (nth 2 (split-string (alist-get 'args (process-attributes dtach-client))))))
              (setq dtach-daemon (car pids)))
            (setq pids (cdr pids)))))

      ;; Get children.
      (if (null dtach-daemon)
          (message "Cannot find associated dtach daemon")
        ;; (if (string= (alist-get 'comm (process-attributes dtach-daemon)) "dtach")
        (setq bash-root (alist-get dtach-daemon ppids))
        ;; (setq bash-root dtach-daemon ppids
        ;;       dtach-daemon nil))
        (push bash-root result)
        ;; Add all recursive children of bash-root to result.
        (let ((l 0))
          (while (/= l (length ppids))
            (setq l (length ppids))
            (let (newppids)
              (while ppids
                (when (member (caar ppids) result)
                  (push (cdar ppids)  result))
                (pop ppids))
              (setq ppids newppids)))))
      (message "bash tree %s" result)
      ;; (sort result '<) ; Debug only
      result)))

(defun eshell-detach-stop ()
  (interactive)
  ;; TODO: TSTP? STOP?
  (eshell-detach-signal 'STOP (eshell-detach-graph)))

(defun eshell-detach-continue ()
  (interactive)
  (eshell-detach-signal 'CONT (eshell-detach-graph)))

;; Inpsired by `helm-top-sh'.
(defun eshell-detach-signal (sig pids)
  "Run `kill' shell command with signal SIG on PIDs."
  (message "kill -%s %s exited with status %s"
           sig (mapconcat 'number-to-string pids " ")
           (apply #'call-process
                  "kill" nil nil nil (format "-%s" sig) (mapcar 'number-to-string pids))))

(provide 'package-eshell-detach)
