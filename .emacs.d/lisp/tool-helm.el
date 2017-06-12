;;; Helm

(when (require 'helm-descbinds nil t)
  (helm-descbinds-mode))

(when (require 'wgrep-helm nil t)
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-enable-key (kbd "C-x C-q")))

;;; Require helm-ls-git unconditionally, this makes following config easier.
(require 'helm-ls-git)

(helm-mode 1)
;; (helm-autoresize-mode 1)
(setq helm-follow-mode-persistent t)
(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
;; (setq helm-split-window-default-side 'right)
(setq helm-reuse-last-window-split-state t)
(setq helm-findutils-search-full-path t)
(setq helm-display-header-line nil)

(setq helm-apropos-fuzzy-match t)
(setq helm-buffers-fuzzy-matching t)
(setq helm-imenu-fuzzy-match t)
(setq helm-M-x-fuzzy-match t)

;;; Add bindings to `helm-apropos`.
;;; https://github.com/emacs-helm/helm/issues/1140
(defun helm-def-source--emacs-commands (&optional default)
  (helm-build-in-buffer-source "Commands"
    :init `(lambda ()
             (helm-apropos-init 'commandp ,default))
    :fuzzy-match helm-apropos-fuzzy-match
    :filtered-candidate-transformer (and (null helm-apropos-fuzzy-match)
                                         'helm-apropos-default-sort-fn)
    :candidate-transformer 'helm-M-x-transformer-1
    :nomark t
    :action '(("Describe Function" . helm-describe-function)
              ("Find Function" . helm-find-function)
              ("Info lookup" . helm-info-lookup-symbol))))

;;; Make `helm-mini' almighty.
(setq helm-mini-default-sources '(helm-source-buffers-list
                                  helm-source-recentf
                                  helm-source-ls-git
                                  helm-source-bookmarks
                                  helm-source-bookmark-set
                                  helm-source-buffer-not-found))

;;; `helm-source-ls-git' must be defined manually.
;;; See https://github.com/emacs-helm/helm-ls-git/issues/34.
(setq helm-source-ls-git
      (and (memq 'helm-source-ls-git helm-ls-git-default-sources)
           (helm-make-source "Git files" 'helm-ls-git-source
             :fuzzy-match helm-ls-git-fuzzy-match)))

;;; Eshell
(add-hook
 'eshell-mode-hook
 (lambda ()
   (eshell-cmpl-initialize)
   (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
   (define-key eshell-mode-map (kbd "C-r") 'helm-eshell-history)))

;;; Do not exclude any files from 'git grep'.
(setq helm-grep-git-grep-command "git --no-pager grep -n%cH --color=always --full-name -e %p -- %f")

;;; Use `pt' instead of `ag'.
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
    (helm-do-grep-ag nil)))

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
(define-key mickey-minor-mode-map (kbd "C-x C-f") 'helm-find-files)
(define-key mickey-minor-mode-map (kbd "C-x c C-/") 'helm-find)
(define-key mickey-minor-mode-map (kbd "C-x C-b") 'helm-mini)
(define-key mickey-minor-mode-map (kbd "M-y") 'helm-show-kill-ring)
(define-key mickey-minor-mode-map (kbd "C-x C-x") 'helm-mark-or-exchange-rect)
(define-key mickey-minor-mode-map (kbd "M-s o") 'helm-occur)
(define-key mickey-minor-mode-map (kbd "C-h a") 'helm-apropos)
(define-key mickey-minor-mode-map (kbd "C-M-%") 'helm-regexp)
(define-key mickey-minor-mode-map (kbd "C-x M-g") 'helm-grep-git-or-ag)
(define-key mickey-minor-mode-map (kbd "C-x M-G") 'helm-do-grep-ag)
(define-key mickey-minor-mode-map (kbd "C-x M-b") 'helm-resume) ; Convenient for god-mode.
(define-key helm-find-files-map (kbd "C-c C-/") 'helm-ff-run-find-sh-command) ; Convenient for god-mode.

;;; We use the M-s prefix just like `occur'.
(define-key prog-mode-map "\M-sf" 'helm-semantic-or-imenu)
;;; The text-mode-map binding targets structured text modes like Markdown.
(define-key text-mode-map "\M-sf" 'helm-semantic-or-imenu)
(with-eval-after-load "org"
  (define-key org-mode-map "\M-sf" 'helm-org-in-buffer-headings))

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

(setq helm-source-names-using-follow '("Occur" "Git-Grep" "PT" "mark-ring" "Org Headings"))

(provide 'tool-helm)
