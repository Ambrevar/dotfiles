;;; Emacs config

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Prerequisites

(let ((minver "24.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))

;;; Speed up init.
;;; Temporarily reduce garbage collection during startup. Inspect `gcs-done'.
(defun reset-gc-cons-threshold ()
  (setq gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value))))
(setq gc-cons-threshold (* 64 1024 1024))
(add-hook 'after-init-hook 'reset-gc-cons-threshold)
;;; Temporarily disable the file name handler.
(setq default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(defun reset-file-name-handler-alist ()
  (setq file-name-handler-alist default-file-name-handler-alist))
(add-hook 'after-init-hook 'reset-file-name-handler-alist)

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

;;; Assembly
(push 'nasm-mode package-selected-packages)

;;; Asymptote
(add-to-list 'load-path "/usr/share/asymptote")
(autoload 'asy-mode "asy-mode.el" "Asymptote major mode." t)
(autoload 'lasy-mode "asy-mode.el" "Hybrid Asymptote/Latex major mode." t)
(autoload 'asy-insinuate-latex "asy-mode.el" "Asymptote insinuate LaTeX." t)
(add-to-list 'auto-mode-alist '("\\.asy$" . asy-mode))

;;; BBCode
(nconc package-selected-packages '(bbcode-mode))
(with-eval-after-load 'bbcode-mode (require 'init-bbcode))

;;; Bibtex
(setq bibtex-entry-format '(opts-or-alts required-fields numerical-fields whitespace realign last-comma delimiters braces sort-fields))
(setq bibtex-field-delimiters 'double-quotes)
(add-hook 'bibtex-mode-hook 'turn-off-indent-tabs)

;;; Bison/Flex
(nconc package-selected-packages '(bison-mode))

;;; C/C++
(with-eval-after-load 'cc-mode (require 'init-cc))

;;; ChangeLog
(defun change-log-set-indent-rules ()
  (setq tab-width 2 left-margin 2))
(add-hook 'change-log-mode-hook 'change-log-set-indent-rules)

;;; Company
(nconc package-selected-packages '(company helm-company))
(when (require 'company nil t)
  (setq company-idle-delay nil))

;;; Debbugs
(nconc package-selected-packages '(debbugs))

;;; Dired
;;; Dired is loaded after init.el, so configure it later.
(with-eval-after-load 'dired (require 'init-dired))

;;; Evil
(nconc package-selected-packages '(evil evil-leader evil-ediff evil-magit evil-mc evil-mc-extras linum-relative))
(when (require 'evil nil t) (require 'init-evil))

;;; Eshell
;;; Extend completion.
(nconc package-selected-packages '(pcomplete-extension))
(with-eval-after-load 'eshell (require 'init-eshell))

;;; Esup
(nconc package-selected-packages '(esup))

;;; GLSL
(nconc package-selected-packages '(glsl-mode))

;;; Go
(nconc package-selected-packages '(go-mode go-eldoc go-guru go-rename helm-go-package company-go))
(with-eval-after-load 'go-mode (require 'init-go))

;;; Graphviz dot
;; The view command is broken but the preview command works (it displays the PNG
;; in a new window), which is enough and probably not worth a fix.
(nconc package-selected-packages '(graphviz-dot-mode))

;;; GUD (GDB, etc.)
(with-eval-after-load 'gud (require 'init-gud))
;;; JavaScript
(add-hook 'js-mode-hook (lambda () (defvaralias 'js-indent-level 'tab-width)))

;;; Helm
(nconc package-selected-packages '(helm helm-descbinds helm-ls-git))
(when (require 'helm-config nil t) (require 'init-helm))

;;; Indentation engine fix.
(require 'smiext "init-smiext")

;;; Indentation style guessing.
;; (nconc 'package-selected-packages '(dtrt-indent))

;;; Lisp
;;; Should not use tabs.
(dolist (hook '(lisp-mode-hook emacs-lisp-mode-hook))
  (add-hook hook 'turn-on-fmt-before-save)
  (add-hook hook 'turn-off-indent-tabs))
(define-key lisp-mode-shared-map "\M-." 'find-symbol-at-point)
;;; Common LISP
(setq inferior-lisp-program "clisp")

;;; Lua
(nconc package-selected-packages '(lua-mode))
(with-eval-after-load 'lua-mode (require 'init-lua))

;;; Magit
(nconc package-selected-packages '(magit))
(when (require 'magit nil t)
  (set-face-foreground 'magit-branch-remote "orange red")
  (setq git-commit-summary-max-length fill-column)
  (setq magit-diff-refine-hunk 'all)
  (global-set-key (kbd "C-x g") 'magit-status))

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
(with-eval-after-load 'make-mode (require 'init-makefile))

;;; Markdown
(nconc package-selected-packages '(markdown-mode))
(with-eval-after-load 'markdown-mode (require 'init-markdown))

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
(nconc package-selected-packages '(mediawiki))
(add-to-list 'auto-mode-alist '("\\.wiki\\'" . mediawiki-mode))
(with-eval-after-load 'mediawiki (require 'init-mediawiki))

;;; Org-mode
(with-eval-after-load 'org (require 'init-org))

;;; PDF
;;; TODO: Replace with pdf-tools package?
(autoload 'pdf-view "init-pdf" nil t)
(autoload 'pdf-compress "init-pdf" nil t)

;;; Perl
(defun perl-set-indent-rules ()
  (defvaralias 'perl-indent-level 'tab-width))
(defun perl-set-compiler ()
  (setq compile-command (concat "perl " (shell-quote-argument buffer-file-name))))
(add-hook 'perl-mode-hook 'perl-set-indent-rules)
(add-hook 'perl-mode-hook 'perl-set-compiler)

;;; po
(nconc package-selected-packages '(po-mode))

;;; Python
(with-eval-after-load 'python (require 'init-python))

;;; Roff / Nroff
(with-eval-after-load 'nroff-mode (require 'init-nroff))

;;; Shell
(with-eval-after-load 'sh-script (require 'init-sh))
;;; Arch Linux PKGBUILD
(add-to-list 'auto-mode-alist '("PKGBUILD" . sh-mode))
;;; rc
(add-to-list 'auto-mode-alist '("rc\\'" . sh-mode))
;;; Fish
(nconc package-selected-packages '(fish-mode))
(defun fish-check-buffer ()
  (when (string-match "/tmp/tmp\..*\.fish" (buffer-file-name))
    (when (require 'with-editor nil t) (with-editor-mode))
    (end-of-line)))
(add-hook 'find-file-hook 'fish-check-buffer)

;;; Srt (subtitles)
(add-to-list 'auto-mode-alist '("\\.srt\\'" . text-mode))

;;; StackExchange
(nconc package-selected-packages '(sx))
(with-eval-after-load 'sx
  (setq sx-cache-directory (concat emacs-cache-folder "sx")))

;;; TeX / LaTeX / Texinfo
(with-eval-after-load 'tex-mode (require 'init-tex))
(with-eval-after-load 'texinfo (require 'init-texinfo))
;;; LaTeX is defined in the same file as TeX. To separate the loading, we add it
;;; to the hook.
(add-hook 'latex-mode-hook (lambda () (require 'init-latex)))
(nconc package-selected-packages '(latex-math-preview latex-pretty-symbols))

;;; Torrent
(nconc package-selected-packages '(tranmission))

;;; Translator
;;; TODO: Find alternative package.
(autoload 'itranslate "init-itranslate" nil t)
(autoload 'itranslate-lines "init-itranslate" nil t)
(autoload 'itranslate-region "init-itranslate" nil t)

;;; Web forms.
;;; Remove auto-fill in web edits because wikis and forums do not like it.
;;; This works for qutebrowser, but may need changes for other browsers.
(defun browser-check-buffer ()
  (when (string-match (concat (getenv "BROWSER") "-editor-*") (buffer-name))
    (when (require 'with-editor nil t) (with-editor-mode))
    (auto-fill-mode -1)))
(add-hook 'find-file-hook 'browser-check-buffer)

;;; Wgrep
(nconc package-selected-packages '(wgrep-helm wgrep-pt))
(when (require 'wgrep nil t)
  (set-face-attribute 'wgrep-face nil :inherit 'ediff-current-diff-C :foreground 'unspecified :background 'unspecified :box nil))

;;; XML / SGML
(defun sgml-setup ()
  (setq sgml-xml-mode t)
  ;; (toggle-truncate-lines) ; This seems to slow down Emacs.
  (turn-off-auto-fill))
(add-hook 'sgml-mode-hook 'sgml-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Finalization

;;; Don't let `customize' clutter my config.
(setq custom-file (expand-file-name "custom.el" server-socket-dir))
(load custom-file t)

;;; We need this at the very end to make sure it doesn't get overriden by other
;;; minor modes.
(mickey-minor-mode 1)

;;; Local config. You can use it to set system specific variables, such as the
;;; external web browser or pdf viewer. You can also backport features for an
;;; old Emacs. For instance:
;;
;; (defun comment-line... ;; From emacs 25
;;
;; (transient-mark-mode 1)
;;
;; ;; Fix slow startup when network is slow. Most visible with Helm and Magit with Emacs <25.
;; (setq tramp-ssh-controlmaster-options "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")

(load "init-local" t t)
