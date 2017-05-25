;; Helm

(when (require 'helm-descbinds nil t)
  (helm-descbinds-mode))

(when (require 'wgrep-helm nil t)
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-enable-key (kbd "C-x C-q")))

(require 'helm-ls-git nil t)
(helm-mode 1)
; (helm-autoresize-mode 1)
(setq helm-follow-mode-persistent t)
(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
;; (setq helm-split-window-default-side 'right)
(setq helm-reuse-last-window-split-state t)
(setq helm-findutils-search-full-path t)

(setq helm-apropos-fuzzy-match t)
(setq helm-buffers-fuzzy-matching t)
(setq helm-imenu-fuzzy-match t)
(setq helm-M-x-fuzzy-match t)

;; Do not exclude any files from 'git grep'.
(setq helm-grep-git-grep-command "git --no-pager grep -n%cH --color=always --full-name -e %p -- %f")

;; Use `pt' instead of `ag'.
(setq helm-grep-ag-command "pt -e -S --hidden --color --nogroup %s %s %s")

(defun helm-grep-git-or-ag (arg)
  "Run `helm-grep-do-git-grep' if possible; fallback to `helm-do-grep-ag' otherwise.
Requires `call-process-to-string' from `functions'."
  (interactive "P")
  (require 'vc)
  (require 'functions)
  (if (and (vc-find-root default-directory ".git")
           (or arg (split-string (call-process-to-string "git" "ls-files" "-z") "\0" t)))
      (helm-grep-do-git-grep arg)
    (helm-do-grep-ag)))

(defun helm-grep-git-all-or-ag ()
  "Run `helm-grep-do-git-grep' over all git files."
  (interactive)
  (helm-grep-do-git-grep t))

(defun helm-mark-or-exchange-rect ()
  "Run `helm-all-mark-rings-before-mark-point' or `rectangle-exchange-point-and-mark' if in rectangle-mark-mode."
  (interactive)
  (if rectangle-mark-mode
      (rectangle-exchange-point-and-mark)
    (helm-all-mark-rings)))

(define-key mickey-minor-mode-map (kbd "M-x") 'helm-M-x)
(define-key mickey-minor-mode-map (kbd "C-x M-f") 'helm-semantic-or-imenu)
(define-key mickey-minor-mode-map (kbd "C-x C-f") 'helm-find-files)
(define-key mickey-minor-mode-map (kbd "C-x c C-/") 'helm-find)
(define-key mickey-minor-mode-map (kbd "C-x C-d") 'helm-browse-project)
(define-key mickey-minor-mode-map (kbd "C-x b") 'helm-buffers-list)
(define-key mickey-minor-mode-map (kbd "C-x C-b") 'helm-mini)
(define-key mickey-minor-mode-map (kbd "M-y") 'helm-show-kill-ring)
(define-key mickey-minor-mode-map (kbd "C-x C-x") 'helm-mark-or-exchange-rect)
(define-key mickey-minor-mode-map (kbd "M-s o") 'helm-occur)
(define-key mickey-minor-mode-map (kbd "C-h a") 'helm-apropos)
(define-key mickey-minor-mode-map (kbd "C-M-%") 'helm-regexp)
(define-key mickey-minor-mode-map (kbd "C-x M-g") 'helm-grep-git-or-ag)
(define-key mickey-minor-mode-map (kbd "C-x M-G") 'helm-do-grep-ag)
(define-key mickey-minor-mode-map (kbd "C-x C-r") 'helm-filtered-bookmarks)
(define-key mickey-minor-mode-map (kbd "C-x M-b") 'helm-resume) ; Convenient for god-mode.
(define-key helm-find-files-map (kbd "C-c C-/") 'helm-ff-run-find-sh-command) ; Convenient for god-mode.

(set-face-background 'helm-source-header "white")
(set-face-foreground 'helm-source-header "black")
(set-face-background 'helm-selection "#4f4f4f")
(set-face-background 'helm-visible-mark "#2f2f2f")
(set-face-foreground 'helm-visible-mark nil)
(set-face-foreground 'helm-match "red")
(set-face-attribute 'helm-buffer-file nil :background 'unspecified :foreground "white" :weight 'normal)
(set-face-attribute 'helm-buffer-directory nil :background 'unspecified :foreground "#1e90ff" :weight 'bold)
(set-face-attribute 'helm-ff-directory nil :background 'unspecified :foreground 'unspecified :weight 'unspecified :inherit 'helm-buffer-directory)
(set-face-attribute 'helm-ff-file nil :background 'unspecified :foreground 'unspecified :weight 'unspecified :inherit 'helm-buffer-file)

(setq helm-source-names-using-follow '("Occur" "Git-Grep" "PT" "mark-ring"))

(provide 'tool-helm)
