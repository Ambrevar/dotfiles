;;==============================================================================
;; Shell
;;==============================================================================

;; TODO: set sh-shell-file per buffer?

;; Allow comment indentation.
(setq sh-indent-comment t)

(add-hook
 'sh-mode-hook
 (lambda ()
   (set (make-local-variable 'compile-command) (concat sh-shell-file " " buffer-file-name))) )

(run-mode-hooks 'sh-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(define-skeleton sh-while-getopts
  "Insert a getops prototype."
  "optstring: "
  > "_printhelp ()
{
    cat<<EOF
Usage: ${1##*/} [OPTIONS] FILES

Options:

  -h:  Show this help.

EOF
}" \n
\n
> "while getopts :" str " OPT; do" \n
> "case $OPT in" \n
'(setq v1 (append (vconcat str) nil))
( (prog1 (if v1 (char-to-string (car v1)))
    (if (eq (nth 1 v1) ?:)
        (setq v1 (nthcdr 2 v1)
              v2 "\"$OPTARG\"")
      (setq v1 (cdr v1)
            v2 nil)))
  > str ")" \n
  > _ v2 " ;;" \n)
> "?)" \n
> "_printhelp \"$0\"" \n
"exit 1 ;;"  > \n
"esac" > \n
"done" > \n \n
"shift $(($OPTIND - 1))" \n
"if [ $# -eq 0 ]; then
    _printhelp \"$0\"
    exit 1
fi" \n)

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

(provide 'mode-sh)
