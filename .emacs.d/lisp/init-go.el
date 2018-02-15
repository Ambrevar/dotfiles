;;; Go

;;; REVIEW: We should not need to use `use-local-map' here.
;;; Reported at https://github.com/dominikh/go-mode.el/issues/191.
(use-local-map go-mode-map)

(ambrevar/local-set-keys
 "C-c m" 'ambrevar/go-main
 "C-c D" 'godoc
 "C-c d" 'godoc-at-point
 "M-." #'godef-jump
 "<f5>" 'ambrevar/go-metalinter
 "C-<f5>" 'ambrevar/go-metalinter-command)
(when (require 'helm-go-package nil t)
  (local-set-key (kbd "C-c D") 'helm-go-package))

(when (require 'company-go nil t)
  (add-hook 'go-mode-hook 'company-mode)
  (add-to-list 'company-backends 'company-go)
  (local-set-key (kbd "M-<tab>") (if (require 'helm-company nil t) 'helm-company 'company-complete)))

(setq gofmt-command "goimports")
(setq godoc-command "godoc -ex")
(setq godoc-and-godef-command "godoc -ex")

(defvar-local ambrevar/gometalinter-args
  (mapconcat
   'identity
   '("--cyclo-over=20 --deadline=20s"
     ;; Ignore some benign errors.
     "-e 'declaration of \"?err\"? shadows'"
     "-e 'error return value not checked \\(.*\\.Close\\(\\)'"
     ;; Customize linters.
     "-E misspell"
     "-E unparam"
     "-E unused"
     ;; gofmt is only useful if not called on save with '-s'
     ;; (goimports does not do this) and for its first rule not
     ;; superseded by gosimple or solint:
     ;; https://golang.org/cmd/gofmt/#hdr-The_simplify_command
     "-E gofmt")
   " ") "Additional arguments to pass to gometalinter.")

(defun ambrevar/go-metalinter (arg)
  "Run gometalinter.
With prefix argument, prompt for commandline."
  (interactive "P")
  (let ((compile-command  (format "gometalinter %s" ambrevar/gometalinter-args)))
    (if arg
        (call-interactively 'compile)
      (compile compile-command))))

(defun ambrevar/go-metalinter-command ()
  "Prompt for gometalinter commandline and run it."
  (interactive)
  (ambrevar/go-metalinter t))

(defun ambrevar/go-set-compile-command ()
  "Set `compile-command' depending on the context.

- go install: file is in GOPATH and is not a test file.
- go test: file is in GOPATH and is a test file.
- go run `buffer-file-name': file is not in GOPATH.

Note that the -cover test flag is left out since it shifts line numbers."
  (interactive)
  (setq compile-command
        (if (ambrevar/go-buffer-in-gopath-p)
            (if (string-match "_test.[gG][oO]$" buffer-file-name)
                "go test -v -run ."
              "go install")
          (concat "go run " (shell-quote-argument buffer-file-name)))))

(defun ambrevar/go-buffer-in-gopath-p ()
  (if (not buffer-file-name)
      nil
    (let ((dir (expand-file-name (file-name-directory buffer-file-name))) (looping t) (gopath (split-string (getenv "GOPATH") ":")))
      (while (progn
               (if (member dir gopath)
                   (setq looping nil)
                 (setq dir (expand-file-name ".." dir)))
               (and looping (not (string= dir "/")))))
      (if (string= dir "/") nil t))))

(when (require 'go-guru nil t)
  (unless (executable-find "guru")
    ;; Requires `call-process-to-string' from `functions'."
    (require 'functions)
    (setq go-guru-command
          (concat (replace-regexp-in-string "\n$" "" (ambrevar/call-process-to-string "go" "env" "GOTOOLDIR")) "/guru"))))

(defun ambrevar/go-turn-on-gofmt-before-save ()
  (add-hook 'before-save-hook #'gofmt-before-save nil t))

(add-hook 'go-mode-hook 'ambrevar/go-turn-on-gofmt-before-save)

(when (require 'go-eldoc nil t)
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(add-hook 'go-mode-hook 'ambrevar/go-set-compile-command)

(defun ambrevar/godoc-setup ()
  (setq tab-width 8))

(add-hook 'godoc-mode-hook 'ambrevar/godoc-setup)

(define-skeleton ambrevar/go-main
  "Insert main function with basic includes."
  nil
  > "package main" "\n" \n
  "import (" \n
  "\"fmt\"" \n
  ")" > "\n" \n
  "func main() {" \n
  > @ _ \n
  "}" > \n)

(provide 'init-go)
