(when (require 'helm-descbinds nil t)
  (helm-descbinds-mode))

(require 'helm-ls-git nil t)
(helm-mode 1)
; (helm-autoresize-mode 1)
(setq helm-follow-mode-persistent t)
(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
;; (setq helm-split-window-default-side 'right)
(setq helm-reuse-last-window-split-state t)

(setq helm-apropos-fuzzy-match t)
(setq helm-buffers-fuzzy-matching t)
(setq helm-imenu-fuzzy-match t)
(setq helm-M-x-fuzzy-match t)

;; Do not exclude any files from 'git grep'.
(setq helm-grep-git-grep-command "git --no-pager grep -n%cH --color=always --full-name -e %p -- %f")

;; Use `pt' instead of `ag'.
(setq helm-grep-ag-command "pt -S --hidden --color --nogroup %s %s %s")

(defun helm-grep-git-or-ag (arg)
  "Run `helm-grep-do-git-grep' if possible; fallback to `helm-do-grep-ag' otherwise."
  (interactive "P")
  (require 'vc)
  (if (vc-find-root default-directory ".git")
      (helm-grep-do-git-grep arg)
    (helm-do-grep-ag arg)))

(define-key mickey-minor-mode-map (kbd "M-x") 'helm-M-x)
(define-key mickey-minor-mode-map (kbd "C-x M-f") 'helm-semantic-or-imenu)
(define-key mickey-minor-mode-map (kbd "C-x C-/") 'helm-find)
(when (require 'helm-fuzzy-find nil t)
  (define-key mickey-minor-mode-map (kbd "C-c C-/") 'helm-fuzzy-find))
(define-key mickey-minor-mode-map (kbd "C-x C-f") 'helm-find-files)
(define-key mickey-minor-mode-map (kbd "C-x C-d") 'helm-browse-project)
(define-key mickey-minor-mode-map (kbd "C-x b") 'helm-buffers-list)
(define-key mickey-minor-mode-map (kbd "C-x C-b") 'helm-mini)
(define-key mickey-minor-mode-map (kbd "M-y") 'helm-show-kill-ring)
(define-key mickey-minor-mode-map (kbd "C-x C-x") 'helm-all-mark-rings)
(define-key mickey-minor-mode-map (kbd "C-x x") 'helm-mark-ring)
(define-key mickey-minor-mode-map (kbd "M-s o") 'helm-occur)
(define-key mickey-minor-mode-map (kbd "C-h a") 'helm-apropos)
(define-key mickey-minor-mode-map (kbd "C-M-%") 'helm-regexp)
(define-key mickey-minor-mode-map (kbd "C-x C-g") 'helm-grep-git-or-ag)
(define-key mickey-minor-mode-map (kbd "C-x r b") 'helm-filtered-bookmarks)

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
;; Helm color for unsaved buffers? Dim special *buffers*?

(provide 'tool-helm)
