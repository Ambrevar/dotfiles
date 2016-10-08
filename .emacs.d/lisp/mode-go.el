;;==============================================================================
;; Go
;;==============================================================================

(setq gofmt-command "goimports")
(setq godoc-command "godoc -ex")
(setq godoc-and-godef-command "godoc -ex")

(defun go-set-compile-command ()
  "Set `compile-command' depending on the context.
If `compile-command' is not \"go build\":
- go install: file is in GOPATH and is not a test file.
- go test: file is in GOPATH and is a test file.
- go build: file is not in GOPATH.
If `compile-command' is \"go build\":
- go run <buffer-file-name>."
  (interactive)
  (set (make-local-variable 'compile-command)
       (if (string= compile-command "go build")
           (concat "go run " (shell-quote-argument buffer-file-name))
         (if (go-buffer-in-gopath-p) (if (string-match "_test.[gG][oO]$" buffer-file-name) "go test -v" "go install")  "go build")))
  (message "Set `compile-command' to `%s'" compile-command))

(defun go-buffer-in-gopath-p ()
  (if (not buffer-file-name)
      nil
    (let ((dir (expand-file-name (file-name-directory buffer-file-name))) (looping t) (gopath (split-string (getenv "GOPATH") ":")))
      (while (progn
               (if (member dir gopath)
                   (setq looping nil)
                 (setq dir (expand-file-name ".." dir)))
               (and looping (not (string= dir "/")))))
      (if (string= dir "/") nil t))))

(defun go-metalinter ()
  (interactive)
  (let ((compile-command "gometalinter --cyclo-over=20 --deadline=20s -e 'declaration of err shadows' -e 'error return value not checked \\(.*\\.Close\\(\\)'"))
  (compile compile-command)))

(when (require 'go-guru nil t)
  (unless (executable-find "guru")
    ; Requires `call-process-to-string' from `functions'."
    (require 'functions)
    (setq go-guru-command
          (concat (replace-regexp-in-string "\n$" "" (call-process-to-string "go" "env" "GOTOOLDIR")) "/guru"))))

(add-hook
 'godoc-mode-hook
 (lambda ()
   (setq tab-width 8)))

(add-hook-and-eval
 'go-mode-hook
 (lambda ()
   (add-hook 'before-save-hook #'gofmt-before-save nil t)
   (when (require 'go-eldoc nil t)
     (go-eldoc-setup))
   (when (require 'company-go nil t)
     (add-to-list 'company-backends 'company-go)
     (company-mode)
     (if (fboundp 'helm-company)
         (local-set-key (kbd "M-TAB") 'helm-company)
       (local-set-key (kbd "M-TAB") 'company-complete)))
   (local-set-key (kbd "C-c m") 'go-main)
   (local-set-key (kbd "C-c D") 'godoc)
   (when (require 'helm-go-package nil t)
     (local-set-key (kbd "C-c D") 'helm-go-package))
   (local-set-key (kbd "C-c d") 'godoc-at-point)
   (local-set-key (kbd "M-.") #'godef-jump)
   (local-set-key (kbd "C-<f10>") 'go-set-compile-command)
   (local-set-key (kbd "<f9>") 'go-metalinter)
   (go-set-compile-command)))

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
