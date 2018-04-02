;;; Helm

;;; TODO: helm-ff should allow opening several marks externally, e.g.  sxiv for
;;; pics. See
;;; https://github.com/emacs-helm/helm/wiki/Find-Files#open-files-externally.
;;; What about the default program?  It currently defaults to ~/.mailcap, which is
;;; not so customizable.  Would ranger's rifle be useful here?  See
;;; https://github.com/emacs-helm/helm/issues/1796.  There is the `openwith' package.

;;; TODO: Batch-open torrent files automatically.  Add to mailcap?  Same as
;;; above, C-c C-x does not allow for opening several files at once.

;;; TODO: helm-find in big folders sometimes leads bad results, like exact match
;;; not appearing first. Better sorting?

;;; TODO: Implement alternating-color multiline lists.
;;; See https://github.com/emacs-helm/helm/issues/1790.

(when (< emacs-major-version 26)
  (when (require 'linum-relative nil t)
    (helm-linum-relative-mode 1)))

(when (require 'helm-descbinds nil t)
  (helm-descbinds-mode))

(when (require 'wgrep-helm nil t)
  (setq wgrep-auto-save-buffer t
        wgrep-enable-key (kbd "C-x C-q")))

(when (require 'helm-ls-git nil t)
  ;; `helm-source-ls-git' must be defined manually.
  ;; See https://github.com/emacs-helm/helm-ls-git/issues/34.
  (setq helm-source-ls-git
        (and (memq 'helm-source-ls-git helm-ls-git-default-sources)
             (helm-make-source "Git files" 'helm-ls-git-source
               :fuzzy-match helm-ls-git-fuzzy-match))))

(helm-mode 1)
;; (helm-autoresize-mode 1)
(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

;;; This makes the copy and rename operations asynchronous.
(dired-async-mode)

;;; Generic configuration.
(setq
 helm-follow-mode-persistent t
 helm-reuse-last-window-split-state t
 helm-display-header-line nil
 helm-findutils-search-full-path t
 helm-show-completion-display-function nil
 helm-completion-mode-string ""
 helm-dwim-target 'completion
 helm-echo-input-in-header-line t
 helm-use-frame-when-more-than-two-windows nil

 helm-apropos-fuzzy-match t
 helm-buffers-fuzzy-matching t
 helm-eshell-fuzzy-match t
 helm-imenu-fuzzy-match t
 helm-M-x-fuzzy-match t
 helm-recentf-fuzzy-match t

 ;; https://github.com/emacs-helm/helm/issues/1910
 helm-buffers-end-truncated-string "…"
 helm-buffer-max-length 22

 helm-window-show-buffers-function 'helm-window-mosaic-fn
 helm-window-prefer-horizontal-split t)

(defun ambrevar/helm-split-window-combined-fn (window)
  "Helm window splitting that combined most standard features.

- With C-u, split inside. With C-u C-u, use same window.
- Else use biggest other window when available.
- Else split horizontally if width>height, vertically otherwise."
  (cond
   ((or (minibufferp helm-current-buffer)
        (and
         (not (one-window-p t))
         (not (equal current-prefix-arg '(4)))
         (not (equal current-prefix-arg '(16)))))
    ;; Find biggest window.
    (let (biggest (maxarea 0))
      (dolist (w (window-list))
        (unless (eq w (selected-window))
          (let ((area (* (window-pixel-width w) (window-pixel-height w))))
            (when (> area maxarea)
              (setq maxarea area
                    biggest w)))))
      biggest))
   ((equal current-prefix-arg '(16))
    ;; Same window.
    (selected-window))
   (t
    ;; If split inside or if unique window.
    (split-window (selected-window) nil
                  (if (> (window-pixel-width) (window-pixel-height))
                      'right
                    'below)))))
(setq helm-split-window-preferred-function 'ambrevar/helm-split-window-combined-fn)

;;; Add bindings to `helm-apropos`. TODO: Does not work most of the times.
;;; https://github.com/emacs-helm/helm/issues/1140
(defun ambrevar/helm-def-source--emacs-commands (&optional default)
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
(require 'helm-bookmark)
(setq helm-mini-default-sources `(helm-source-buffers-list
                                  helm-source-recentf
                                  ,(when (boundp 'helm-source-ls-git) 'helm-source-ls-git)
                                  helm-source-bookmarks
                                  helm-source-bookmark-set
                                  helm-source-buffer-not-found))

;;; Eshell
(defun ambrevar/helm/eshell-set-keys ()
  (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
  (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history)
  (define-key eshell-mode-map (kbd "M-s") nil) ; Useless when we have 'helm-eshell-history.
  (define-key eshell-mode-map (kbd "M-s f") 'helm-eshell-prompts-all))
(add-hook 'eshell-mode-hook 'ambrevar/helm/eshell-set-keys)

;;; TODO: Use helm-ff history in helm file completion.
;;; https://github.com/emacs-helm/helm/issues/1118
;; (define-key helm-read-file-map (kbd "M-p") 'helm-ff-run-switch-to-history)

;;; Do not exclude any files from 'git grep'.
(setq helm-grep-git-grep-command "git --no-pager grep -n%cH --color=always --full-name -e %p -- %f")

(defun ambrevar/helm-grep-git-or-ag (arg)
  "Run `helm-grep-do-git-grep' if possible; fallback to `helm-do-grep-ag' otherwise.
Requires `call-process-to-string' from `functions'."
  (interactive "P")
  (require 'vc)
  (require 'functions)
  (if (and (vc-find-root default-directory ".git")
           (or arg (split-string (ambrevar/call-process-to-string "git" "ls-files" "-z") "\0" t)))
      (helm-grep-do-git-grep arg)
    (helm-do-grep-ag nil)))

(defun ambrevar/helm-grep-git-all-or-ag ()
  "Run `helm-grep-do-git-grep' over all git files."
  (interactive)
  (helm-grep-do-git-grep t))

(defun ambrevar/helm-mark-or-exchange-rect ()
  "Run `helm-all-mark-rings-before-mark-point' or `rectangle-exchange-point-and-mark' if in rectangle-mark-mode."
  (interactive)
  (if rectangle-mark-mode
      (rectangle-exchange-point-and-mark)
    (helm-all-mark-rings)))

(global-set-key [remap execute-extended-command] 'helm-M-x)
(global-set-key [remap find-file] 'helm-find-files)
(global-set-key [remap occur] 'helm-occur)
(global-set-key [remap list-buffers] 'helm-mini)
;; (global-set-key [remap dabbrev-expand] 'helm-dabbrev)
(global-set-key [remap yank-pop] 'helm-show-kill-ring)
;;; Do not remap 'exchange-point-and-mark, Evil needs it in visual mode.
(global-set-key (kbd "C-x C-x") 'ambrevar/helm-mark-or-exchange-rect)
(global-set-key [remap apropos-command] 'helm-apropos)
(global-set-key [remap query-replace-regexp] 'helm-regexp)
(unless (boundp 'completion-in-region-function)
  (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
  (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))

(ambrevar/global-set-keys
 "C-x M-g" 'ambrevar/helm-grep-git-or-ag
 "C-x M-G" 'helm-do-grep-ag)

;;; Use the M-s prefix just like `occur'.
(define-key prog-mode-map (kbd "M-s f") 'helm-semantic-or-imenu)
;;; The text-mode-map binding targets structured text modes like Markdown.
(define-key text-mode-map (kbd "M-s f") 'helm-semantic-or-imenu)
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "M-s f") 'helm-org-in-buffer-headings))

(set-face-attribute 'helm-source-header nil :inherit 'header-line :height 'unspecified :background 'unspecified :foreground 'unspecified)
(set-face-background 'helm-selection "#4f4f4f")
(set-face-background 'helm-visible-mark "#2f2f2f")
(set-face-foreground 'helm-visible-mark nil)
(set-face-foreground 'helm-match "red")
(set-face-attribute 'helm-buffer-file nil :background 'unspecified :foreground "white" :weight 'normal)
(set-face-attribute 'helm-buffer-directory nil :background 'unspecified :foreground "#1e90ff" :weight 'bold)
(set-face-attribute 'helm-ff-directory nil :background 'unspecified :foreground 'unspecified :weight 'unspecified :inherit 'helm-buffer-directory)
(set-face-attribute 'helm-ff-file nil :background 'unspecified :foreground 'unspecified :weight 'unspecified :inherit 'helm-buffer-file)
(set-face-foreground 'helm-grep-finish "#00AA00")

(setq helm-source-names-using-follow '("Occur" "Git-Grep" "AG" "mark-ring" "Org Headings"))

;;; From https://www.reddit.com/r/emacs/comments/5q922h/removing_dot_files_in_helmfindfiles_menu/.
(defun ambrevar/helm-skip-dots (old-func &rest args)
  "Skip . and .. initially in helm-find-files.  First call OLD-FUNC with ARGS."
  (apply old-func args)
  (let ((sel (helm-get-selection)))
    (if (and (stringp sel) (string-match "/\\.$" sel))
        (helm-next-line 2)))
  (let ((sel (helm-get-selection))) ; if we reached .. move back
    (if (and (stringp sel) (string-match "/\\.\\.$" sel))
        (helm-previous-line 1))))
(advice-add #'helm-preselect :around #'ambrevar/helm-skip-dots)
(advice-add #'helm-ff-move-to-first-real-candidate :around #'ambrevar/helm-skip-dots)

(with-eval-after-load 'desktop
  (add-to-list 'desktop-globals-to-save 'kmacro-ring)
  (add-to-list 'desktop-globals-to-save 'last-kbd-macro)
  (add-to-list 'desktop-globals-to-save 'kmacro-counter)
  (add-to-list 'desktop-globals-to-save 'kmacro-counter-format)
  (add-to-list 'desktop-globals-to-save 'helm-ff-history))

(helm-top-poll-mode)
;;; Column indices might need some customizing. See `helm-top-command' and
;;; https://github.com/emacs-helm/helm/issues/1586 and
;;; https://github.com/emacs-helm/helm/issues/1909.

;;; Fallback on 'find' if 'locate' is not available.
(unless (executable-find "locate")
  (setq helm-locate-recursive-dirs-command "find %s -type d -regex .*%s.*$"))

;; See https://github.com/emacs-helm/helm/issues/1962.
(defun ambrevar/helm-locate-create-or-update-db (db root &optional update)
  "See `ambrevar/helm-locate-meta'."
  (let ((was-missing (not (file-exists-p db))))
    (when (or update was-missing)
      (if (= (shell-command
              (or  (executable-find "updatedb-local")
                   (funcall helm-locate-create-db-function
                            db
                            root)))
             0)
          (message "locatedb file `%s' %s" db (if was-missing "created" "updated"))
        (error "Failed to %s locatedb file `%s'" (if was-missing "create" "update") db)))))
(defun ambrevar/helm-locate-meta (&optional update)
  "Like `helm-locate' but also use the databases found in /media and /run/media.
With prefix argument, UPDATE the databases."
  (interactive "P")
  (let ((user-db (expand-file-name "~/.cache/locate.db")))
    (ambrevar/helm-locate-create-or-update-db user-db "/" update)
    (helm-locate-with-db
     (mapconcat 'identity
                (append (list user-db)
                        (mapc
                         (lambda (db)
                           (ambrevar/helm-locate-create-or-update-db db (file-name-directory db) update))
                         (apply 'append (mapcar
                                         (lambda (root) (file-expand-wildcards (concat root "/*/locate.db")))
                                         (list (concat "/run/media/" (user-login-name)) "/media")))))
                ":")
     nil (thing-at-point 'filename))))

;;; Convenience.
(defun ambrevar/helm-toggle-visible-mark-backwards (arg)
  (interactive "p")
  (helm-toggle-visible-mark (- arg)))
(define-key helm-map (kbd "S-SPC") 'ambrevar/helm-toggle-visible-mark-backwards)

(global-set-key  (kbd "C-<f4>") 'helm-execute-kmacro)

(provide 'init-helm)
