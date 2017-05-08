;; Emacs config

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global variables.

(defvar mickey-minor-mode-map (make-keymap)
  "Keymap for mickey-minor-mode. See its docstring for more
details.")

(define-minor-mode mickey-minor-mode
  "The mode's keymap allows for overriding all global and major mode keys.
To view where the bindings are set in your config files, lookup
`mickey-minor-mode-map' over it. Example:

  (define-key mickey-minor-mode-map (kbd \"C-i\") 'some-function)"
  t " my-keys" 'mickey-minor-mode-map)
(add-hook 'minibuffer-setup-hook (lambda () (mickey-minor-mode 0)))

(defvar emacs-cache-folder "~/.cache/emacs/"
  "Cache folder is everything we do not want to track along with
  the configuration files.")
(if (not (file-directory-p emacs-cache-folder))
    (make-directory emacs-cache-folder t))

;; Load config easily.
(add-to-list 'load-path "~/.emacs.d/lisp")

;; Local plugin folder for quick install. All files in this folder will be
;; accessible to Emacs config. This is done to separate the versioned config
;; files from the external packages. For instance you can put package.el in
;; there for Emacs <24.
(add-to-list 'load-path "~/.emacs.d/local")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load main config

(require 'functions nil t)
(require 'main nil t)
(require 'visual nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vanilla

;; Major modes
(add-hook 'c++-mode-hook      (lambda () (require 'mode-cc)))
(add-hook 'c-mode-hook        (lambda () (require 'mode-cc)))
(add-hook 'sgml-mode-hook     (lambda () (require 'mode-sgml)))
(add-hook 'js-mode-hook       (lambda () (defvaralias 'js-indent-level 'tab-width)))
(add-hook 'latex-mode-hook    (lambda () (require 'mode-latex)))
(add-hook 'makefile-mode-hook (lambda () (require 'mode-makefile)))
(add-hook 'nroff-mode-hook    (lambda () (require 'mode-nroff)))
(add-hook 'perl-mode-hook     (lambda () (require 'mode-perl)))
(add-hook 'python-mode-hook   (lambda () (require 'mode-python)))
(add-hook 'sh-mode-hook       (lambda () (require 'mode-sh)))
(add-hook 'tex-mode-hook      (lambda () (require 'mode-tex)))
(add-hook 'texinfo-mode-hook  (lambda () (require 'mode-texinfo)))

;; Minor modes
(add-hook 'dired-mode-hook  (lambda () (require 'mode-dired)))
(add-hook 'eshell-load-hook (lambda () (require 'mode-eshell)))
(add-hook 'gud-mode-hook    (lambda () (require 'mode-gud)))
(add-hook 'octave-mode-hook (lambda () (require 'mode-octave)))
(add-hook 'org-mode-hook    (lambda () (require 'mode-org)))

;; Bind extensions.
(add-to-list 'auto-mode-alist '("rc\\'" . sh-mode)) ; rc shell
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode)) ; matlab

;; Tools
(autoload 'pdf-view "tool-pdf" nil t)
(autoload 'pdf-compress "tool-pdf" nil t)

(autoload 'itranslate "tool-itranslate" nil t)
(autoload 'itranslate-lines "tool-itranslate" nil t)
(autoload 'itranslate-region "tool-itranslate" nil t)

(require 'smiext "tool-smiext")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; External

(when (require 'package nil t)
  ;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
  (setq package-user-dir (concat emacs-cache-folder "elpa"))
  (package-initialize))

;;------------------------------------------------------------------------------
;; External modes

(add-to-list 'load-path "/usr/share/asymptote")
(autoload 'asy-mode "asy-mode.el" "Asymptote major mode." t)
(autoload 'lasy-mode "asy-mode.el" "Hybrid Asymptote/Latex major mode." t)
(autoload 'asy-insinuate-latex "asy-mode.el" "Asymptote insinuate LaTeX." t)
(add-to-list 'auto-mode-alist '("\\.asy$" . asy-mode))

(autoload 'maxima-mode "maxima" "Maxima mode" t)
(autoload 'maxima "maxima" "Maxima interaction" t)
(setq auto-mode-alist (cons '("\\.mac" . maxima-mode) auto-mode-alist))

(load-external "\\.bbcode\\'" 'bbcode-mode)
(add-hook 'bbcode-mode-hook (lambda () (require 'mode-bbcode)))

(load-external "\\.l\\'" 'flex-mode nil 'c-mode)
(load-external "\\.yy?\\'" 'bison-mode nil 'c-mode)

(load-external "\\.vert\\'\\|\\.frag\\'\\|\\.glsl\\'" 'glsl-mode nil 'c-mode)

(add-to-list 'package-selected-packages 'go-mode)
(add-to-list 'package-selected-packages 'go-eldoc)
(add-to-list 'package-selected-packages 'go-guru)
(add-to-list 'package-selected-packages 'go-rename)
(add-to-list 'package-selected-packages 'helm-go-package)
(add-to-list 'package-selected-packages 'company-go)
(load-external "\\.go\\'" 'go-mode)
(add-hook 'go-mode-hook (lambda () (require 'mode-go)))

(load-external "\\.dot\\'" 'graphviz-dot-mode)
(add-hook 'graphviz-dot-mode-hook (lambda () (require 'mode-dot)))

(add-to-list 'package-selected-packages 'lua-mode)
(load-external "\\.lua\\'" 'lua-mode nil 'sh-mode)
(add-hook 'lua-mode-hook (lambda () (require 'mode-lua)))

(add-to-list 'package-selected-packages 'markdown-mode)
(load-external "\\.md\\'\\|\\.markdown\\'" 'markdown-mode)
;; If we need more option, add it to a dedicated file.
(add-hook
 'markdown-mode-hook
 (lambda ()
   (set-face-attribute 'markdown-header-face-1 nil :inherit 'info-title-1)
   (set-face-attribute 'markdown-header-face-2 nil :inherit 'info-title-2)
   (set-face-attribute 'markdown-header-face-3 nil :inherit 'info-title-3)
   (set-face-attribute 'markdown-header-face-4 nil :inherit 'info-title-4)
   (set (make-local-variable 'paragraph-start) "
")))

(load-external "\\.wiki\\'" 'mediawiki 'mediawiki-mode)
(add-hook 'mediawiki-mode-hook (lambda () (require 'mode-mediawiki)))

;; .po support. This mode has no hooks.
(load-external "\\.po\\'\\|\\.po\\." 'po-mode)
(when (fboundp 'po-find-file-coding-system)
  (modify-coding-system-alist 'file "\\.po\\'\\|\\.po\\." 'po-find-file-coding-system))

(add-to-list 'package-selected-packages 'fish-mode)

;;------------------------------------------------------------------------------
;; External tools

;; (autoload 'guess-style-set-variable "guess-style" nil t)
;; (autoload 'guess-style-guess-variable "guess-style")
;; (autoload 'guess-style-guess-all "guess-style" nil t)
;; (setq guess-style-info-mode 1)
;; (add-hook 'prog-mode-hook (lambda () (ignore-errors (guess-style-guess-all))))

(add-to-list 'package-selected-packages 'multiple-cursors)
(add-to-list 'package-selected-packages 'phi-search)
(when (require 'multiple-cursors nil t)
  (setq mc/list-file (concat emacs-cache-folder "mc-lists.el"))
  ;; Load the file at the new location.
  (load mc/list-file t)
  (global-unset-key (kbd "C-<down-mouse-1>"))
  (define-key mickey-minor-mode-map (kbd "C-<mouse-1>") 'mc/add-cursor-on-click)
  (define-key mickey-minor-mode-map (kbd "C-x M-r") 'mc/edit-lines)
  (define-key mickey-minor-mode-map (kbd "C-x M-m") 'mc/mark-more-like-this-extended)
  (define-key mickey-minor-mode-map (kbd "C-x M-l") 'mc/mark-all-like-this-dwim)
  ;; Search compatible with mc.
  (require 'phi-search nil t))

(add-to-list 'package-selected-packages 'helm)
(add-to-list 'package-selected-packages 'helm-descbinds)
(add-to-list 'package-selected-packages 'helm-ls-git)
;; (add-to-list 'package-selected-packages 'helm-pt) ; No need?
(add-to-list 'package-selected-packages 'wgrep-helm)
(add-to-list 'package-selected-packages 'wgrep-pt)
(when (require 'helm-config nil t)
  (require 'tool-helm))

(when (require 'dired+ nil t)
  (toggle-diredp-find-file-reuse-dir 1))

(when (require 'powerline nil t)
  (powerline-default-theme))

(add-to-list 'package-selected-packages 'company)
(add-to-list 'package-selected-packages 'helm-company)
(when (require 'company nil t)
  (setq company-idle-delay nil))

(add-to-list 'package-selected-packages 'magit)
(when (require 'magit nil t)
  (set-face-foreground 'magit-branch-remote "orange red")
  (setq git-commit-summary-max-length fill-column)
  (setq magit-diff-refine-hunk 'all)
  (global-set-key (kbd "C-x g") 'magit-status))

(add-to-list 'package-selected-packages 'god-mode)
(when (require 'god-mode nil t)
  (require 'tool-god))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End of config

;; We need to put it at the end to make sure it doesn't get overriden by other
;; minor modes.
(mickey-minor-mode 1)

;; Local hook. You can use it to set system specific variables, such as the
;; external web browser or pdf viewer. You can also backport feature for old
;; Emacs. For instance:
; (setq pdf-viewer "evince")
; (setq pdf-viewer-args nil)
;
; (mapcar
;  (lambda (mode-hook)
;    (add-hook mode-hook (lambda () (run-hooks 'prog-mode-hook))))
;  '(asm-mode-hook awk-mode-hook c++-mode-hook c-mode-hook
;                  emacs-lisp-mode-hook lisp-mode-hook lua-mode-hook
;                  makefile-mode-hook octave-mode-hook perl-mode-hook
;                  python-mode-hook scheme-mode-hook sh-mode-hook))
;
; (defun comment-line... ;; From emacs 25
;
; (transient-mark-mode 1)
;
; ;; Fix slow startup when network is slow. Most visible with Helm and Magit with Emacs <25.
; (setq tramp-ssh-controlmaster-options "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")

(load "local" t t)
