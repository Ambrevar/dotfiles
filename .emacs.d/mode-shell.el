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
  nil
  "for " (setq str (skeleton-read "Index variable: " "i"))
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

(define-skeleton sh-cmd
  "Insert a line that executes if command is found in path."
  "Command name: "
  "command -v " @ str " >/dev/null 2>&1 && " @ _)

(provide 'mode-shell)
