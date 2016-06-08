;;==============================================================================
;; Go
;;==============================================================================
(add-to-list 'package-pinned-packages 'go-rename)
;; Optional packages: go-autocomplete, go-guru

(defun go-eval-buffer ()
  "Eval buffer with `go run'."
  (interactive)
  (let ((compile-command-backup compile-command))
    (compile (concat "go run " (shell-quote-argument buffer-file-name)))
    (setq compile-command compile-command-backup)))

(defun go-buffer-in-gopath-p ()
  (let ((dir (expand-file-name (file-name-directory buffer-file-name))) (looping t) (gopath (getenv "GOPATH")))
    (while (progn
             (if (string= dir gopath)
                 (setq looping nil)
               (setq dir (expand-file-name ".." dir)))
             (and looping (not (string= dir "/")))))
    (if (string= dir "/") nil t)))

(add-hook
 'godoc-mode-hook
 (lambda ()
   (setq tab-width 8)))

 (add-hook-and-eval
 'go-mode-hook
 (lambda ()
   (setq gofmt-command "goimports")
   (setq godoc-command "godoc -ex")
   (setq godoc-and-godef-command "godoc -ex")
   (add-hook 'before-save-hook #'gofmt-before-save nil t)
   (local-set-key (kbd "C-c m") 'go-main)
   (local-set-key (kbd "C-c D") 'godoc)
   (local-set-key (kbd "C-c d") 'godoc-at-point)
   (local-set-key (kbd "M-.") #'godef-jump)
   (local-set-key (kbd "C-<f10>") 'go-eval-buffer)
   (local-set-key (kbd "<f9>")
                  (lambda () (interactive)
                    (let ((compile-command "gometalinter --cyclo-over=20 --deadline=20s -e 'declaration of err shadows' -e 'error return value not checked \\(.*\\.Close\\(\\)'"))
                      (compile compile-command))))
   (set (make-local-variable 'compile-command)
        (if (go-buffer-in-gopath-p) (if (string-match "_test.[gG][oO]$" buffer-file-name) "go test" "go install")  "go build"))))

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
