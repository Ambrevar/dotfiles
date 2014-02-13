;;==============================================================================
;; Shell
;;==============================================================================

;; Allow comment indentation.
(setq sh-indent-comment t)

(setq sh-shell-file "/bin/sh")

(defun shell-set-interpreter ()
  "If a shabang is present, use it as the shell interpreter,
otherwise use 'sh-shell-file'."
  "Use compile to run python programs."
  (interactive)
  (hack-local-variables)
  (let ((firstline
         (car
          (split-string (buffer-substring-no-properties 1 (point-max)) "\n"))))
    (let ((sh-interpreter
           (if (not (string-match "^#!" firstline))
               sh-shell-file
             (substring firstline 2))))

      (set (make-local-variable 'compile-command)
         (concat sh-interpreter " " buffer-file-name)))))

(add-hook
 'sh-mode-hook
 (lambda ()
   (shell-set-interpreter)))

(define-skeleton sh-for
  "Insert a for loop.  See `sh-feature'. This overrides vanilla function."
  "Index variable: "
  > "for " str | "i"
  '(setq v1 (skeleton-read "Index values: " ""))
  (unless (string= v1 "")
    (concat " in " v1))
  "; do" \n
  > _ \n
  "done" > \n)

(define-skeleton sh-check
  "Insert a function checking for presence in PATH."
  nil
  "check()
{
    for i ; do
        if ! command -v $i >/dev/null 2>&1; then
            echo \"'$i' not found in PATH. Exiting.\" >&2
            exit 1
        fi
    done
}
")

(define-skeleton sh-getopts
  "Insert a getops prototype."
  nil
  "_printhelp ()
{
    cat<<EOF
Usage: ${1##*/} [OPTIONS] FILES

Options:

  -h:  Show this help.

EOF
}

opt_test=false
while getopts ':ht' opt; do
    case $opt in
        h)
            _printhelp \"$0\"
            exit 1 ;;
        t)
            opt_test=true ;;
        ?)
            _printhelp \"$0\"
            exit 1 ;;
    esac
done

shift $(($OPTIND - 1))

if [ $# -eq 0 ]; then
    _printhelp \"$0\"
    exit 1
fi
")

(define-skeleton sh-command
  "Insert a line that executes if command is found in path."
  "Command name: "
  > "command -v " @ str " >/dev/null 2>&1 && " @ _)

(define-skeleton sh-ifcommand
  "Insert a test to check if command is found in path."
  "Command name: "
  > "if command -v " @ str " >/dev/null 2>&1; then" \n
  > @ _ \n
  "fi" > \n)

(define-skeleton sh-command-or-die
  "Insert a line that quits if command is not found in path."
  "Command name: "
  > "if ! command -v " @ str " >/dev/null 2>&1; then" \n
  > "echo '" str " not found in PATH. Exiting.' >&2" \n
  > "exit 1" \n
  "fi" > \n)

(define-skeleton sh-redirect-to-null
  "Insert a null redirection."
  nil
  ">/dev/null 2>&1")

(define-skeleton sh-while-read
  "Insert a while read loop."
  nil
  > "while IFS= read -r i; do" \n
  > @ _ \n
  > "done <<EOF" \n
  > "EOF" \n)

(provide 'mode-shell)
