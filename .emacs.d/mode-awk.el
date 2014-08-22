;;==============================================================================
;; AWK
;;==============================================================================

(add-hook-and-eval
 'awk-mode-hook
 (lambda ()
   (local-set-key (kbd "C-c C-f") 'awk-fori)
   (local-set-key (kbd "C-c f") 'awk-forin)
   (local-set-key (kbd "C-c C-i") 'awk-if)
   (local-set-key (kbd "C-c (") 'awk-function)
   (local-set-key (kbd "C-c C-p") 'awk-printf)
   (set (make-local-variable 'compile-command) (concat "awk " buffer-file-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-skeleton awk-fori
  "for i loop."
  nil
  > "for (" @ (skeleton-read "" "i = 0") "; " @ (skeleton-read "" "i < N") "; " @ (skeleton-read "" "i++") ") {" \n
  @ _ \n
  "}" > )

(define-skeleton awk-forin
  "for in loop."
  nil
  > "for (" @ (skeleton-read "" "var") " in " @ (skeleton-read "" "array") ") {" \n
  @ _ \n
  "}" > )


(define-skeleton awk-function
  "Insert struction for function"
  nil
  "function " @ (skeleton-read "Name: ") " (" @ (skeleton-read "Args: ") ") {" \n
  > _ \n
  "}" > \n)

(define-skeleton awk-if
  "Insert an if statement."
  "Condition: "
  > "if (" @ str ") {" \n
  > @ _ \n
  ("Other condition, %s: "
   "} else if (" > @ str ") {" \n
   @ \n)
  "} else {" > \n
  @ \n
  resume:
  "}" > \n)

(define-skeleton awk-printf
  "fprintf/printf snippet.
If no file descriptor is provided, switch do printf.  The format
string is properly parsed (%% are not taken into account).\n
Requires `count-percents'."
  nil
  '(require 'functions)
  "printf ("
  "\"" (setq v1 (skeleton-read "Format string: " "%s\\n")) "\""
  '(setq v2 (count-percents v1))
  '(setq v1 0)
  '(while (< v1 v2)
     (setq v1 (1+ v1))
     (skeleton-insert '(nil (concat ", " (skeleton-read "Value: ")))))
  @ ")")

(provide 'mode-awk)
