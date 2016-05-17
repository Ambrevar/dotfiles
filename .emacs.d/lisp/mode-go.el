;;==============================================================================
;; Go
;;==============================================================================

(require 'go-autocomplete nil t)

(defun go-eval-buffer ()
  "Eval buffer with `go run'."
  (interactive)
  (let ((compile-command-backup compile-command))
    (compile (concat "go run " (shell-quote-argument buffer-file-name)))
    (setq compile-command compile-command-backup)))

(add-hook-and-eval
 'go-mode-hook
 (lambda ()
   (add-hook 'before-save-hook #'gofmt-before-save nil t)
   (local-set-key (kbd "C-c m") 'go-main)
   (local-set-key (kbd "C-c d") 'godoc)
   (local-set-key (kbd "M-.") #'godef-jump)
   (local-set-key (kbd "C-<f10>") 'go-eval-buffer)
   (set (make-local-variable 'compile-command) "go install")))

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
