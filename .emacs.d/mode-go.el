;;==============================================================================
;; Go
;;==============================================================================

(add-hook-and-eval
 'go-mode-hook
 (lambda ()
   (add-hook 'before-save-hook #'gofmt-before-save nil t)
   (local-set-key (kbd "C-c m") 'go-main)
   (set (make-local-variable 'compile-command) (concat "go run " buffer-file-name))))

(define-skeleton go-main
  "Insert main function with basic includes."
  nil
  > "package main

import (
	\"fmt\"
)

func main () {" \n
> @ _ \n
"}" > \n)


(provide 'mode-go)
