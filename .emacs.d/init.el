;; Emacs config

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prerequisites

(defvar mickey-minor-mode-map (make-keymap) "Keymap for `mickey-minor-mode'.")
(define-minor-mode mickey-minor-mode
  "The mode's keymap allows for overriding all global and major mode keys.
To view where the bindings are set in your config files, lookup
`mickey-minor-mode-map' over it. Example:\n
  (define-key mickey-minor-mode-map (kbd \"C-i\") 'some-function)"
  t " myckey" 'mickey-minor-mode-map)
(add-hook 'minibuffer-setup-hook (lambda () (mickey-minor-mode 0)))

(defvar emacs-cache-folder "~/.cache/emacs/"
  "Cache folder is everything we do not want to track along with
  the configuration files.")
(if (not (file-directory-p emacs-cache-folder))
    (make-directory emacs-cache-folder t))

;; Store additional config in a 'lisp' subfolder and add it to the load path so
;; that `require' can find the files.
(add-to-list 'load-path "~/.emacs.d/lisp")

;; Local plugin folder for quick install. All files in this folder will be
;; accessible to Emacs config. This is done to separate the versioned config
;; files from the external packages. For instance you can put package.el in
;; there for Emacs <24.
(add-to-list 'load-path "~/.emacs.d/local")

(when (require 'package nil t)
  ;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
  (setq package-user-dir (concat emacs-cache-folder "elpa"))
  (package-initialize))

(require 'functions nil t)
(require 'main nil t)
(require 'visual nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Major modes

;; Assembly
(add-to-list 'package-selected-packages 'nasm-mode)

;; Asymptote
(add-to-list 'load-path "/usr/share/asymptote")
(autoload 'asy-mode "asy-mode.el" "Asymptote major mode." t)
(autoload 'lasy-mode "asy-mode.el" "Hybrid Asymptote/Latex major mode." t)
(autoload 'asy-insinuate-latex "asy-mode.el" "Asymptote insinuate LaTeX." t)
(add-to-list 'auto-mode-alist '("\\.asy$" . asy-mode))

;; BBCode
(add-to-list 'package-selected-packages 'bbcode-mode)
(load-external "\\.bbcode\\'" 'bbcode-mode)
(add-hook 'bbcode-mode-hook (lambda () (require 'mode-bbcode)))

;; Bibtex
(setq bibtex-entry-format '(opts-or-alts required-fields numerical-fields whitespace realign last-comma delimiters braces sort-fields))
(setq bibtex-field-delimiters 'double-quotes)
(add-hook 'bibtex-mode-hook (lambda () (setq indent-tabs-mode nil)))

;; Bison/Flex
(load-external "\\.l\\'" 'flex-mode nil 'c-mode)
(load-external "\\.yy?\\'" 'bison-mode nil 'c-mode)

;; C/C++
(add-hook 'c-mode-hook   (lambda () (require 'mode-cc)))
(add-hook 'c++-mode-hook (lambda () (require 'mode-cc)))

;; ChangeLog
(add-hook 'change-log-mode-hook (lambda () (setq tab-width 2 left-margin 2)))

;; GLSL
(load-external "\\.vert\\'\\|\\.frag\\'\\|\\.glsl\\'" 'glsl-mode nil 'c-mode)

;; Go
(add-to-list 'package-selected-packages 'go-mode)
(add-to-list 'package-selected-packages 'go-eldoc)
(add-to-list 'package-selected-packages 'go-guru)
(add-to-list 'package-selected-packages 'go-rename)
(add-to-list 'package-selected-packages 'helm-go-package)
(add-to-list 'package-selected-packages 'company-go)
(load-external "\\.go\\'" 'go-mode)
(add-hook 'go-mode-hook (lambda () (require 'mode-go)))

;; Graphviz dot
(load-external "\\.dot\\'" 'graphviz-dot-mode)
(add-hook 'graphviz-dot-mode-hook (lambda () (require 'mode-dot)))

;; JavaScript
(add-hook 'js-mode-hook (lambda () (defvaralias 'js-indent-level 'tab-width)))

;; Lisp
;; Should not use tabs.
(dolist (hook '(lisp-mode-hook emacs-lisp-mode-hook))
   (add-hook hook (lambda () (setq indent-tabs-mode nil))))
(define-key lisp-mode-shared-map "\M-." 'find-symbol-at-point)
;; Common LISP
(setq inferior-lisp-program "clisp")

;; Lua
(add-to-list 'package-selected-packages 'lua-mode)
(load-external "\\.lua\\'" 'lua-mode nil 'sh-mode)
(add-hook 'lua-mode-hook (lambda () (require 'mode-lua)))

;; Mail with Mutt support.
(add-hook 'mail-mode-hook 'mail-text)
(add-to-list 'auto-mode-alist '("/tmp/mutt-.*" . mail-mode))
(add-hook
 'find-file-hook
 (lambda ()
   (when (and (string-match "/tmp/mutt-.*" (buffer-file-name))
              (require 'with-editor nil t))
     ;; Just like git commits.
     (with-editor-mode))))

;; Makefile
(add-hook 'makefile-mode-hook (lambda () (require 'mode-makefile)))

;; Markdown
(add-to-list 'package-selected-packages 'markdown-mode)
(load-external "\\.md\\'\\|\\.markdown\\'" 'markdown-mode)
;; If we need more option, add it to a dedicated file.
(when (require 'markdown-mode nil t)
	(set-face-attribute 'markdown-header-face-1 nil :inherit 'info-title-1)
	(set-face-attribute 'markdown-header-face-2 nil :inherit 'info-title-2)
	(set-face-attribute 'markdown-header-face-3 nil :inherit 'info-title-3)
	(set-face-attribute 'markdown-header-face-4 nil :inherit 'info-title-4)
	(define-key markdown-mode-map "\M-'" 'markdown-blockquote-region)
	(add-hook 'markdown-mode-hook (lambda () (set (make-local-variable 'paragraph-start) "
"))))

;; Matlab / Octave
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode)) ; matlab
;; Set comments to be '%' to be matlab-compatible.
(add-hook 'octave-mode-hook (lambda () (set (make-local-variable 'comment-start) "% ")))

;; Maxima
(autoload 'maxima-mode "maxima" "Maxima mode" t)
(autoload 'maxima "maxima" "Maxima interaction" t)
(setq auto-mode-alist (cons '("\\.mac" . maxima-mode) auto-mode-alist))

;; Mediawiki
(add-to-list 'package-selected-packages 'mediawiki)
(load-external "\\.wiki\\'" 'mediawiki 'mediawiki-mode)
(add-hook 'mediawiki-mode-hook (lambda () (require 'mode-mediawiki)))

;; Org-mode
(add-hook 'org-mode-hook (lambda () (require 'mode-org)))

;; Perl
(add-hook
 'perl-mode-hook
 (lambda ()
   (defvaralias 'perl-indent-level 'tab-width)
   (setq compile-command (concat "perl " (shell-quote-argument buffer-file-name)))))

;; po
;; No hook for this mode?
(load-external "\\.po\\'\\|\\.po\\." 'po-mode)
(when (fboundp 'po-find-file-coding-system)
  (modify-coding-system-alist 'file "\\.po\\'\\|\\.po\\." 'po-find-file-coding-system))

;; Python
(add-hook 'python-mode-hook (lambda () (require 'mode-python)))

;; Roff / Nroff
(add-hook 'nroff-mode-hook (lambda () (require 'mode-nroff)))

;; Shell
(add-hook 'sh-mode-hook (lambda () (require 'mode-sh)))
;; Arch Linux PKGBUILD
(add-to-list 'auto-mode-alist '("PKGBUILD" . sh-mode))
;; rc
(add-to-list 'auto-mode-alist '("rc\\'" . sh-mode))
;; Fish
(add-to-list 'package-selected-packages 'fish-mode)
(add-hook
 'find-file-hook
 (lambda ()
   (when (and (string-match "/tmp/tmp\..*\.fish" (buffer-file-name))
              (require 'with-editor nil t))
     ;; Just like git commits.
     (with-editor-mode))))

;; Srt (subtitles)
(add-to-list 'auto-mode-alist '("\\.srt\\'" . text-mode))

;; TeX / LaTeX / Texinfo
(add-hook 'tex-mode-hook      (lambda () (require 'mode-tex)))
(add-hook 'texinfo-mode-hook  (lambda () (require 'mode-texinfo)))
(add-hook 'latex-mode-hook    (lambda () (require 'mode-latex)))
(add-to-list 'package-selected-packages 'latex-math-preview)
(add-to-list 'package-selected-packages 'latex-pretty-symbols)
(require 'latex-pretty-symbols nil t)

;; Web forms.
;; Remove auto-fill in web edits because wikis and forums do not like it.
;; This works for qutebrowser, but may need changes for other browsers.
(add-hook
 'find-file-hook
 (lambda ()
   (when (string-match (concat (getenv "BROWSER") "-editor-*") (buffer-name))
     (when (require 'with-editor nil t)
       ;; Just like git commits.
       (with-editor-mode))
     (auto-fill-mode -1))))

;; XML / SGML
(add-hook
 'sgml-mode-hook
 (lambda ()
   (setq sgml-xml-mode t)
   ;; (toggle-truncate-lines) ; This seems to slow down Emacs.
   (turn-off-auto-fill)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Minor modes and features.

;; Company
(add-to-list 'package-selected-packages 'company)
(add-to-list 'package-selected-packages 'helm-company)
(when (require 'company nil t)
  (setq company-idle-delay nil))

;; Dired
(add-hook 'dired-mode-hook  (lambda () (require 'mode-dired)))

;; Eshell
(add-hook 'eshell-load-hook (lambda () (require 'mode-eshell)))

;; Evil
(add-to-list 'package-selected-packages 'evil)
(add-to-list 'package-selected-packages 'evil-leader)
(add-to-list 'package-selected-packages 'evil-magit)
(add-to-list 'package-selected-packages 'evil-mc)
(add-to-list 'package-selected-packages 'evil-mc-extras)
(add-to-list 'package-selected-packages 'linum-relative)
(when (require 'evil nil t)
  (require 'tool-evil))

;; God mode
;; (add-to-list 'package-selected-packages 'god-mode)
;; (when (require 'god-mode nil t)
;;   (require 'tool-god))

;; GUD (GDB, etc.)
(add-hook 'gud-mode-hook    (lambda () (require 'mode-gud)))

;; Helm
(add-to-list 'package-selected-packages 'helm)
(add-to-list 'package-selected-packages 'helm-descbinds)
(add-to-list 'package-selected-packages 'helm-ls-git)
;; (add-to-list 'package-selected-packages 'helm-pt) ; No need?
(add-to-list 'package-selected-packages 'wgrep-helm)
(add-to-list 'package-selected-packages 'wgrep-pt)
(when (require 'helm-config nil t)
  (require 'tool-helm))

;; Indentation engine fix.
(require 'smiext "tool-smiext")

;; Indent style guessing.
(require 'dtrt-indent nil t)

;; Magit
(add-to-list 'package-selected-packages 'magit)
(when (require 'magit nil t)
  (set-face-foreground 'magit-branch-remote "orange red")
  (setq git-commit-summary-max-length fill-column)
  (setq magit-diff-refine-hunk 'all)
  (global-set-key (kbd "C-x g") 'magit-status))

;; Multiple cursors
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

;; PDF
(autoload 'pdf-view "tool-pdf" nil t)
(autoload 'pdf-compress "tool-pdf" nil t)

;; Translator
(autoload 'itranslate "tool-itranslate" nil t)
(autoload 'itranslate-lines "tool-itranslate" nil t)
(autoload 'itranslate-region "tool-itranslate" nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finalization

;; Don't let `customize' clutter my config.
;; This will prompt "File exists, but cannot be read".
(setq custom-file "/dev/null")

;; We need to put it at the end to make sure it doesn't get overriden by other
;; minor modes.
(mickey-minor-mode 1)

;; Local hook. You can use it to set system specific variables, such as the
;; external web browser or pdf viewer. You can also backport feature for old
;; Emacs. For instance:
; (setq pdf-viewer "evince")
; (setq pdf-viewer-args nil)
;
; (dolist (hook '(asm-mode-hook awk-mode-hook c++-mode-hook c-mode-hook
;                 emacs-lisp-mode-hook lisp-mode-hook lua-mode-hook
;                 makefile-mode-hook octave-mode-hook perl-mode-hook
;                 python-mode-hook scheme-mode-hook sh-mode-hook))
;   (add-hook hook (lambda () (run-hooks 'prog-mode-hook))))
;
; (defun comment-line... ;; From emacs 25
;
; (transient-mark-mode 1)
;
; ;; Fix slow startup when network is slow. Most visible with Helm and Magit with Emacs <25.
; (setq tramp-ssh-controlmaster-options "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")

(load "local" t t)
