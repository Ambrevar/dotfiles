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

;;; TODO: Rename eshell-detach and hide implementation details around "dtach".

(defvar eshell-dtach-redraw-method nil
  "If nil, use the default value.
Value must be a string.
See dtach(1) for possible values.")

(defvar eshell-dtach-shell "bash"
  "Shell to run the command in.
Should be bash-compatible.
The end command will be

  \"`eshell-dtach-shell' -c { { <command>; } > >(tee stdout) } 2> >(tee stderr) | tee stdout+stderr\"")

(defvar eshell-dtach-detach-character "^\\"
  "Charcter to press to detach dtach, i.e. leave the process run in the background.
The character syntax follows terminal notations, not Emacs.")

(defvar eshell-dtach-detach-character-binding "C-\\"
  "The Emacs binding matching `eshell-dtach-detach-character'.")

(defvar eshell-dtach-stdout-ext ".stdout"
  "If non-nil and a string, stdout will also be saved to file named after the socket with this extension appened.
The 'tee' program is required.")

(defvar eshell-dtach-stderr-ext ".stderr"
  "If non-nil and a string, stderr will also be saved to file named after the socket with this extension appened.
The 'tee' program is required.")

(defvar eshell-dtach-stdout+stderr-ext ".stdout+stderr"
  "If non-nil and a string, stdout and stderr will also be saved to file named after the socket with this extension appened.
The 'tee' program is required.")

;; TODO: Make it work with pipes.
;; TODO: Test with quotes.
;; TODO: Hook should be on named commands as it should not run for Elisp code.
;; `eshell-named-command-hook' seems to be the way to go.  What about
;; `eshell-rewrite-command-hook'?
(defun eshell-detach-send-input-function (command args)
  "If no other rewriting rule transforms TERMS, assume a named command."
  (message "ESHELL %s %S" command args)
  (remove-hook 'eshell-named-command-hook 'eshell-detach-send-input-function)
  ;; Since sockets get killed on termination, there won't be any leftover if there is no log.  Then it could be useful to _not_ create a subdir.  Let's not do it then.
  ;; TODO: Do not create log if empty.  Workaround: delete 0-byte files.
  (let* ((temporary-file-directory (if server-socket-dir server-socket-dir temporary-file-directory))
         ;; TODO: temp-file should not exist for dtach to start?  That forces us to use make-temp-file, there could be a race condition.
         (socket (make-temp-name
                  (expand-file-name
                   (concat "dtach-"
                           (replace-regexp-in-string "[^A-Za-z0-9]" "_" (concat command "_" (mapconcat 'identity args "_")))
                           "-" (format-time-string "%F-%R:%S") "-")
                   temporary-file-directory)))

         (stdout (if eshell-dtach-stdout-ext (concat socket eshell-dtach-stdout-ext) nil))
         (stderr (if eshell-dtach-stderr-ext (concat socket eshell-dtach-stderr-ext) nil))
         (stdout+stderr (if eshell-dtach-stdout+stderr-ext (concat socket eshell-dtach-stdout+stderr-ext) nil))

         ;; The following test command was inspired by
         ;; https://stackoverflow.com/questions/21465297/tee-stdout-and-stderr-to-separate-files-while-retaining-them-on-their-respective
         ;; { { echo stdout; echo stderr >&2; } > >(tee stdout.txt ); } 2> >(tee stderr.txt ) | tee stdout+stderr.txt

         ;; TODO: Use 'tee -a', cause then we don't
         ;; lose anything when running in the background.  If stdout and stderr can refer
         ;; to the same files. Or can they?  Yes, with 'tee -a'
         (commandline (format "{ { %s; }%s }%s %s"
                              (concat command " " (mapconcat 'identity args " "))
                              (if stdout (format " > >(tee %s );" stdout) "")
                              (if stderr (format " 2> >(tee %s )" stderr) "")
                              (if stdout+stderr (format " | tee %s" stdout+stderr) ""))))
    (message "%s"
             (list
              "dtach"
              "-c" socket "-z"
              eshell-dtach-shell "-c" commandline))
    (throw 'eshell-replace-command
           (eshell-parse-command  "dtach" (cons "-c"
                                                (list
                                                 socket "-z"
                                                 eshell-dtach-shell "-c" commandline))))))

(defun eshell-detach-send-input ()
  (interactive)
  (add-hook 'eshell-named-command-hook 'eshell-detach-send-input-function)
  (eshell-send-input))

;;; TODO: See if `make-process' is the way to go: it supports stderr/stdout separation and stop/cont.

;;; TODO: Re-use `eshell-gather-process-output'?  Re-implement?

;; TODO: But how to pause/resume?
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
