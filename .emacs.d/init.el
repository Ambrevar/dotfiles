;;; Emacs config

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Prerequisites

(defvar mickey-minor-mode-map (make-keymap) "Keymap for `mickey-minor-mode'.")
(define-minor-mode mickey-minor-mode
  "The mode's keymap allows for overriding all global and major mode keys.
To view where the bindings are set in your config files, lookup
`mickey-minor-mode-map' over it. Example:\n
  (define-key mickey-minor-mode-map (kbd \"C-i\") 'some-function)"
  t " mickey" 'mickey-minor-mode-map)
(add-hook 'minibuffer-setup-hook (lambda () (mickey-minor-mode 0)))

(defvar emacs-cache-folder "~/.cache/emacs/"
  "Cache folder is everything we do not want to track along with
  the configuration files.")
(if (not (file-directory-p emacs-cache-folder))
    (make-directory emacs-cache-folder t))

;;; Store additional config in a 'lisp' subfolder and add it to the load path so
;;; that `require' can find the files.
(add-to-list 'load-path "~/.emacs.d/lisp")

;;; Local plugin folder for quick install. All files in this folder will be
;;; accessible to Emacs config. This is done to separate the versioned config
;;; files from the external packages. For instance you can put package.el in
;;; there for Emacs <24.
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
;;; Major modes

;;; Assembly
(add-to-list 'package-selected-packages 'nasm-mode)

;;; Asymptote
(add-to-list 'load-path "/usr/share/asymptote")
(autoload 'asy-mode "asy-mode.el" "Asymptote major mode." t)
(autoload 'lasy-mode "asy-mode.el" "Hybrid Asymptote/Latex major mode." t)
(autoload 'asy-insinuate-latex "asy-mode.el" "Asymptote insinuate LaTeX." t)
(add-to-list 'auto-mode-alist '("\\.asy$" . asy-mode))

;;; BBCode
(add-to-list 'package-selected-packages 'bbcode-mode)
(with-eval-after-load 'bbcode-mode (require 'mode-bbcode))

;;; Bibtex
(setq bibtex-entry-format '(opts-or-alts required-fields numerical-fields whitespace realign last-comma delimiters braces sort-fields))
(setq bibtex-field-delimiters 'double-quotes)
(add-hook 'bibtex-mode-hook 'turn-off-indent-tabs)

;;; Bison/Flex
(add-to-list 'package-selected-packages 'bison-mode)

;;; C/C++
(with-eval-after-load 'cc-mode (require 'mode-cc))

;;; ChangeLog
(defun change-log-set-indent-rules ()
  (setq tab-width 2 left-margin 2))
(add-hook 'change-log-mode-hook 'change-log-set-indent-rules)

;;; GLSL
(add-to-list 'package-selected-packages 'glsl-mode)

;;; Go
(add-to-list 'package-selected-packages 'go-mode)
(add-to-list 'package-selected-packages 'go-eldoc)
(add-to-list 'package-selected-packages 'go-guru)
(add-to-list 'package-selected-packages 'go-rename)
(add-to-list 'package-selected-packages 'helm-go-package)
(add-to-list 'package-selected-packages 'company-go)
(with-eval-after-load 'go-mode (require 'mode-go))

;;; Graphviz dot
;; The view command is broken but the preview command works (it displays the PNG
;; in a new window), which is enough and probably not worth a fix.
(add-to-list 'package-selected-packages 'graphviz-dot-mode)

;;; JavaScript
(add-hook 'js-mode-hook (lambda () (defvaralias 'js-indent-level 'tab-width)))

;;; Lisp
;;; Should not use tabs.
(dolist (hook '(lisp-mode-hook emacs-lisp-mode-hook))
  (add-hook hook 'turn-on-fmt-before-save)
  (add-hook hook 'turn-off-indent-tabs))
(define-key lisp-mode-shared-map "\M-." 'find-symbol-at-point)
;;; Common LISP
(setq inferior-lisp-program "clisp")

;;; Lua
(add-to-list 'package-selected-packages 'lua-mode)
(with-eval-after-load 'lua-mode (require 'mode-lua))

;;; Mail with Mutt support.
(add-hook 'mail-mode-hook 'mail-text)
(add-to-list 'auto-mode-alist '("/tmp/mutt-.*" . mail-mode))
(defun mutt-backup-buffer ()
  "Create a copy of the current buffer.
This is useful for recovery in case Mutt hangs before sending the
e-mail."
  (when (not (boundp 'mutt-backup))
    (set (make-local-variable 'mutt-backup) (make-temp-file (concat (buffer-name) "-"))))
  (copy-file buffer-file-name mutt-backup t))
(defun mutt-check-buffer ()
  (when (string-match "/tmp/mutt-.*" (buffer-file-name))
    ;; Just like magit commits.
    (when (require 'with-editor nil t) (with-editor-mode))
    (add-hook 'after-save-hook 'mutt-backup-buffer nil t)))
(add-hook 'find-file-hook 'mutt-check-buffer)

;;; Makefile
(with-eval-after-load 'make-mode (require 'mode-makefile))

;;; Markdown
(add-to-list 'package-selected-packages 'markdown-mode)
(with-eval-after-load 'markdown-mode
  ;; TODO: Move config to a dedicated file.
  (set-face-attribute 'markdown-header-face-1 nil :inherit 'info-title-1)
  (set-face-attribute 'markdown-header-face-2 nil :inherit 'info-title-2)
  (set-face-attribute 'markdown-header-face-3 nil :inherit 'info-title-3)
  (set-face-attribute 'markdown-header-face-4 nil :inherit 'info-title-4)
  (define-key markdown-mode-map "\M-'" 'markdown-blockquote-region)
  (add-hook 'markdown-mode-hook 'turn-on-newline-paragraph))

;;; Matlab / Octave
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode)) ; matlab
(defun octave-set-comment-start ()
  "Set comment character to '%' to be Matlab-compatible."
  (set (make-local-variable 'comment-start) "% "))
(add-hook 'octave-mode-hook 'octave-set-comment-start)

;;; Maxima
(autoload 'maxima-mode "maxima" "Maxima mode" t)
(autoload 'maxima "maxima" "Maxima interaction" t)
(add-to-list 'auto-mode-alist '("\\.mac" . maxima-mode))

;;; Mediawiki
(add-to-list 'package-selected-packages 'mediawiki)
(add-to-list 'auto-mode-alist '("\\.wiki\\'" . mediawiki-mode))
(with-eval-after-load 'mediawiki (require 'mode-mediawiki))

;;; Org-mode
(with-eval-after-load 'org (require 'mode-org))

;;; Perl
(defun perl-set-indent-rules ()
  (defvaralias 'perl-indent-level 'tab-width))
(defun perl-set-compiler ()
  (setq compile-command (concat "perl " (shell-quote-argument buffer-file-name))))
(add-hook 'perl-mode-hook 'perl-set-indent-rules)
(add-hook 'perl-mode-hook 'perl-set-compiler)

;;; po
(add-to-list 'package-selected-packages 'po-mode)

;;; Python
(with-eval-after-load 'python (require 'mode-python))

;;; Roff / Nroff
(with-eval-after-load 'nroff-mode (require 'mode-nroff))

;;; Shell
(with-eval-after-load 'sh-script (require 'mode-sh))
;;; Arch Linux PKGBUILD
(add-to-list 'auto-mode-alist '("PKGBUILD" . sh-mode))
;;; rc
(add-to-list 'auto-mode-alist '("rc\\'" . sh-mode))
;;; Fish
(add-to-list 'package-selected-packages 'fish-mode)
(defun fish-check-buffer ()
  (when (string-match "/tmp/tmp\..*\.fish" (buffer-file-name))
    (when (require 'with-editor nil t) (with-editor-mode))
    (end-of-line)))
(add-hook 'find-file-hook 'fish-check-buffer)

;;; Srt (subtitles)
(add-to-list 'auto-mode-alist '("\\.srt\\'" . text-mode))

;;; TeX / LaTeX / Texinfo
(with-eval-after-load 'tex-mode (require 'mode-tex))
(with-eval-after-load 'texinfo (require 'mode-texinfo))
;;; LaTeX is defined in the same file as TeX. To separate the loading, we add it
;;; to the hook.
(add-hook 'latex-mode-hook (lambda () (require 'mode-latex)))
(add-to-list 'package-selected-packages 'latex-math-preview)
(add-to-list 'package-selected-packages 'latex-pretty-symbols)
(require 'latex-pretty-symbols nil t)

;;; Web forms.
;;; Remove auto-fill in web edits because wikis and forums do not like it.
;;; This works for qutebrowser, but may need changes for other browsers.
(defun browser-check-buffer ()
  (when (string-match (concat (getenv "BROWSER") "-editor-*") (buffer-name))
    (when (require 'with-editor nil t) (with-editor-mode))
    (auto-fill-mode -1)))
(add-hook 'find-file-hook 'browser-check-buffer)

;;; XML / SGML
(defun sgml-setup ()
  (setq sgml-xml-mode t)
  ;; (toggle-truncate-lines) ; This seems to slow down Emacs.
  (turn-off-auto-fill))
(add-hook 'sgml-mode-hook 'sgml-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Minor modes and features.

;;; Company
(add-to-list 'package-selected-packages 'company)
(add-to-list 'package-selected-packages 'helm-company)
(when (require 'company nil t)
  (setq company-idle-delay nil))

;;; Dired
;;; Dired is loaded after init.el, so configure it later.
(with-eval-after-load 'dired (require 'mode-dired))

;;; Eshell
;;; Extend completion.
(add-to-list 'package-selected-packages 'pcomplete-extension)
;;; Eshell gets initialized differently.  When eshell.el first gets loaded, only
;;; the core is defined and `eshell-load-hook' is called. For every Eshell
;;; session, `eshell-mode' is run: it resets `eshell-mode-map', it loads
;;; modules, runs their hooks and concludes with `eshell-first-time-mode-hook'
;;; (for the first session only) and `eshell-mode-hook'.
;;;
;;; TODO: Move most features inside (with-eval-after-load 'eshell ...)
;;; `eshell-directory-name' is part of the core.
(with-eval-after-load 'eshell
  (setq eshell-directory-name (concat emacs-cache-folder "eshell")))
;;; The banner is a module.
(with-eval-after-load 'em-banner
  (setq-default eshell-modules-list (delq 'eshell-banner eshell-modules-list)))
(add-hook 'eshell-first-time-mode-hook (lambda () (require 'mode-eshell)))

;;; Evil
(add-to-list 'package-selected-packages 'evil)
(add-to-list 'package-selected-packages 'evil-leader)
(add-to-list 'package-selected-packages 'evil-ediff)
(add-to-list 'package-selected-packages 'evil-magit)
(add-to-list 'package-selected-packages 'evil-mc)
(add-to-list 'package-selected-packages 'evil-mc-extras)
(add-to-list 'package-selected-packages 'linum-relative)
(when (require 'evil nil t) (require 'tool-evil))

;;; GUD (GDB, etc.)
(with-eval-after-load 'gud (require 'mode-gud))

;;; Helm
(add-to-list 'package-selected-packages 'helm)
(add-to-list 'package-selected-packages 'helm-descbinds)
(add-to-list 'package-selected-packages 'helm-ls-git)
;; (add-to-list 'package-selected-packages 'helm-pt) ; No need?
(add-to-list 'package-selected-packages 'wgrep-helm)
(add-to-list 'package-selected-packages 'wgrep-pt)
(when (require 'helm-config nil t) (require 'tool-helm))

;;; Indentation engine fix.
(require 'smiext "tool-smiext")

;;; Indent style guessing.
;; (add-to-list 'package-selected-packages 'dtrt-indent)

;;; Magit
(add-to-list 'package-selected-packages 'magit)
(when (require 'magit nil t)
  (set-face-foreground 'magit-branch-remote "orange red")
  (setq git-commit-summary-max-length fill-column)
  (setq magit-diff-refine-hunk 'all)
  (global-set-key (kbd "C-x g") 'magit-status))

;;; Multiple cursors
;;; TODO: Delete?
;; (add-to-list 'package-selected-packages 'multiple-cursors)
;; (add-to-list 'package-selected-packages 'phi-search)
(when (require 'multiple-cursors nil t)
  (setq mc/list-file (concat emacs-cache-folder "mc-lists.el"))
  ;; Load the file at the new location.
  (load mc/list-file t)
  (define-key mickey-minor-mode-map (kbd "C-<mouse-1>") 'mc/add-cursor-on-click)
  (define-key mickey-minor-mode-map (kbd "C-x M-r") 'mc/edit-lines)
  (define-key mickey-minor-mode-map (kbd "C-x M-m") 'mc/mark-more-like-this-extended)
  (define-key mickey-minor-mode-map (kbd "C-x M-l") 'mc/mark-all-like-this-dwim)
  ;; Search compatible with mc.
  (require 'phi-search nil t))

;;; PDF
(autoload 'pdf-view "tool-pdf" nil t)
(autoload 'pdf-compress "tool-pdf" nil t)

;;; Translator
;; TODO: Find alternative package.
(autoload 'itranslate "tool-itranslate" nil t)
(autoload 'itranslate-lines "tool-itranslate" nil t)
(autoload 'itranslate-region "tool-itranslate" nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Finalization

;;; Don't let `customize' clutter my config.
;;; This will prompt "File exists, but cannot be read".
(setq custom-file "/dev/null")

;;; We need to put it at the end to make sure it doesn't get overriden by other
;;; minor modes.
(mickey-minor-mode 1)

;;; Local hook. You can use it to set system specific variables, such as the
;;; external web browser or pdf viewer. You can also backport features for an
;;; old Emacs. For instance:
;;
;; (setq pdf-viewer "evince")
;; (setq pdf-viewer-args nil)
;;
;; (dolist (hook '(asm-mode-hook awk-mode-hook c++-mode-hook c-mode-hook
;;                 emacs-lisp-mode-hook lisp-mode-hook lua-mode-hook
;;                 makefile-mode-hook octave-mode-hook perl-mode-hook
;;                 python-mode-hook scheme-mode-hook sh-mode-hook))
;;   (add-hook hook (lambda () (run-hooks 'prog-mode-hook))))
;;
;; (defun comment-line... ;; From emacs 25
;;
;; (transient-mark-mode 1)
;;
;; ;; Fix slow startup when network is slow. Most visible with Helm and Magit with Emacs <25.
;; (setq tramp-ssh-controlmaster-options "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")

(load "local" t t)
