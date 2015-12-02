;;==============================================================================
;; Go
;;==============================================================================

(add-hook-and-eval
 'go-mode-hook
 (lambda ()
   (add-hook 'before-save-hook #'gofmt-before-save nil t)
   (local-set-key (kbd "C-c m") 'go-main)
   (local-set-key (kbd "C-c C-d") 'godoc)
   (set (make-local-variable 'compile-command) (concat "go run " (shell-quote-argument buffer-file-name)))))

(define-skeleton go-main
  "Insert main function with basic includes."
  nil
  > "package main" "\n" \n
  "import (" \n
  "\"fmt\"" \n
  ")" > "\n" \n
  "func main() {" \n
  > @ _ \n
  "}" > \n)

(provide 'mode-go)
