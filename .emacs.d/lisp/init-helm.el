;;; Helm

(when (require 'helm-descbinds nil t)
  (helm-descbinds-mode))

(when (require 'wgrep-helm nil t)
  (setq wgrep-auto-save-buffer t
        wgrep-enable-key (kbd "C-x C-q")))

;;; Require helm-ls-git unconditionally, this makes following config easier.
(require 'helm-ls-git)

(helm-mode 1)
;; (helm-autoresize-mode 1)
(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

(setq
 helm-follow-mode-persistent t
 helm-reuse-last-window-split-state t
 helm-display-header-line nil
 helm-findutils-search-full-path t
 helm-show-completion-use-special-display nil

 helm-apropos-fuzzy-match t
 helm-buffers-fuzzy-matching t
 helm-eshell-fuzzy-match t
 helm-imenu-fuzzy-match t
 helm-M-x-fuzzy-match t
 helm-recentf-fuzzy-match t)

;;; From https://github.com/emacs-helm/helm/issues/362.
;;; This is not perfect with evil mode as the cursor type is not right in the header line and the evil cursor remains in the minibufferl
;;; https://emacs.stackexchange.com/questions/17058/change-cursor-type-in-helm-header-line#17097
(setq helm-echo-input-in-header-line t)
(defun helm-hide-minibuffer-maybe ()
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                              `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))
(add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)

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
(defun helm/eshell-set-keys ()
  (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
  (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history))
(add-hook 'eshell-mode-hook 'helm/eshell-set-keys)

;;; Do not exclude any files from 'git grep'.
(setq helm-grep-git-grep-command "git --no-pager grep -n%cH --color=always --full-name -e %p -- %f")

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

(global-set-keys
 "M-x" 'helm-M-x
 "C-x C-f" 'helm-find-files
 "C-x C-b" 'helm-mini
 "M-y" 'helm-show-kill-ring
 "C-x C-x" 'helm-mark-or-exchange-rect
 "M-s o" 'helm-occur
 "C-h a" 'helm-apropos
 "C-M-%" 'helm-regexp
 "C-x M-g" 'helm-grep-git-or-ag
 "C-x M-G" 'helm-do-grep-ag)

;;; We use the M-s prefix just like `occur'.
(define-key prog-mode-map "\M-sf" 'helm-semantic-or-imenu)
;;; The text-mode-map binding targets structured text modes like Markdown.
(define-key text-mode-map "\M-sf" 'helm-semantic-or-imenu)
(with-eval-after-load 'org
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

(setq helm-source-names-using-follow '("Occur" "Git-Grep" "AG" "mark-ring" "Org Headings"))

;;; From https://www.reddit.com/r/emacs/comments/5q922h/removing_dot_files_in_helmfindfiles_menu/.
(defun helm-skip-dots (old-func &rest args)
  "Skip . and .. initially in helm-find-files.  First call OLD-FUNC with ARGS."
  (apply old-func args)
  (let ((sel (helm-get-selection)))
    (if (and (stringp sel) (string-match "/\\.$" sel))
        (helm-next-line 2)))
  (let ((sel (helm-get-selection))) ; if we reached .. move back
    (if (and (stringp sel) (string-match "/\\.\\.$" sel))
        (helm-previous-line 1))))
(advice-add #'helm-preselect :around #'helm-skip-dots)
(advice-add #'helm-ff-move-to-first-real-candidate :around #'helm-skip-dots)

(with-eval-after-load 'desktop
  (add-to-list 'desktop-globals-to-save 'helm-ff-history))

(helm-top-poll-mode)
;;; Column indices might need some customizing. See `helm-top-command' and
;;; https://github.com/emacs-helm/helm/issues/1586.

(provide 'init-helm)
