;;; Sh

(defvaralias 'sh-basic-offset 'tab-width)
(defvaralias 'sh-indentation 'sh-basic-offset)

(setq sh-indent-comment t)

(setq-default sh-shell-file "/bin/sh")
;; (setq-default sh-shell 'sh)

;;; Faces
(set-face-foreground 'sh-heredoc "#00bfff")
(set-face-bold 'sh-heredoc nil)

(defun ambrevar/sh-set-compiler ()
  "Set shell interpreter.
Set `sh-shell', `sh-shell-file' and `compile-command' according to the following rules:
- Look at shabang.
- If file has no name, use default value of sh-shell-file.
- Check extension or file name.
- If none of the above yields a result, use default value of
  sh-shell-file.
The advantages of this function over the vanilla code are:
- You can change default value of sh-shell-file in sh-mode-hook
  and it will be used subsequently.
- Zsh is supported
- compile-command is set.
- Once sh-shell is set, sh-shell-file is changed accordingly. In
  default Emacs, sh-shell-file is always the same."
  (interactive)
  (sh-set-shell
   (cond ((save-excursion
            (goto-char (point-min))
            (looking-at "#![ \t]?\\([^ \t\n]*/bin/env[ \t]\\)?\\([^ \t\n]+\\)"))
          (match-string 2))
         ((not buffer-file-name) sh-shell-file)
         ;; Checks that use `buffer-file-name' follow.
         ((string-match "\\.m?spec\\'" buffer-file-name) "rpm")
         ((string-match "[.]bash\\>"   buffer-file-name) "bash")
         ((string-match "[.]csh\\>"    buffer-file-name) "csh")
         ((string-match "[.]ksh\\>"    buffer-file-name) "ksh")
         ((string-match "[.]sh\\>"     buffer-file-name) "sh")
         ((string-match "[.]zsh\\>"    buffer-file-name) "zsh")
         ((equal (file-name-nondirectory buffer-file-name) ".profile") "sh")
         (t sh-shell-file))
   nil nil)
  ;; Universal version:
  ;; (setq sh-shell-file (executable-find (symbol-name sh-shell)))
  ;; Convenient version:
  (setq sh-shell-file (concat "/bin/" (symbol-name sh-shell)))
  ;; Sometimes with `git merge` it seems that the `buffer-file-name' is not a
  ;; string. We safe-guard that case.
  (when (stringp buffer-file-name)
    (setq compile-command (concat sh-shell-file " " (shell-quote-argument buffer-file-name)))))

(defun ambrevar/sh-set-indent-rules ()
  (setq sh-indent-for-case-label 0
        sh-indent-for-case-alt '+))

(defun ambrevar/sh-set-prompt ()
  (set (make-local-variable 'defun-prompt-regexp)
       (concat "^\\(function[ \t]\\|[[:alnum:]_]+[ \t]+()[ \t]+\\)")))

;;; Hooks
(dolist (fun '(ambrevar/sh-set-indent-rules ambrevar/sh-set-prompt ambrevar/sh-set-compiler))
  (add-hook 'sh-mode-hook fun))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-skeleton ambrevar/sh-commands-or-die
  "Insert a loop that exits if any of the commands is not found in path."
  "Command names: "
  > "for i " @ str "; do" \n
  > "if ! command -v \"$i\" >/dev/null 2>&1; then" \n
  > "echo >&2 \"'$i' not found\"" \n
  > "exit 1" \n
  "fi" > \n
  "done" > \n \n)

(define-skeleton ambrevar/sh-ifcommand
  "Insert a test to check if command is found in path."
  "Command name: "
  > "if command -v " @ str " >/dev/null 2>&1; then" \n
  > @ _ \n
  "fi" > \n)

(define-skeleton ambrevar/sh-while-getopts
  "Insert a getops prototype."
  "optstring: "
  > "usage() {" \n
  > "cat<<EOF" \n
  "Usage: ${1##*/} [OPTIONS] FILES

Options:

  -h:  Show this help.

EOF
}" > \n
\n
> "while getopts :" str " OPT; do" \n
> "case $OPT in" \n
'(setq v1 (append (vconcat str) nil))
((prog1 (if v1 (char-to-string (car v1)))
   (if (eq (nth 1 v1) ?:)
       (setq v1 (nthcdr 2 v1)
             v2 "\"$OPTARG\"")
     (setq v1 (cdr v1)
           v2 nil)))
 > str ")" \n
 > _ v2 " ;;" \n)
> "\\?)" \n
> "usage \"$0\"" \n
"exit 1 ;;"  > \n
"esac" > \n
"done" > \n
\n
"shift $(($OPTIND - 1))" \n
"if [ $# -eq 0 ]; then" \n
> "usage \"$0\"" \n
"exit 1" \n
"fi" > \n)

(define-skeleton ambrevar/sh-while-read
  "Insert a while read loop."
  nil
  > "while IFS= read -r i; do" \n
  > @ _ \n
  "done <<EOF" > \n
  "EOF" > \n)

(provide 'init-sh)
