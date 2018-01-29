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

(defvar emacs-cache-folder "~/.cache/emacs/"
  "Cache folder is everything we do not want to track together
  with the configuration files.")
(if (not (file-directory-p emacs-cache-folder))
    (make-directory emacs-cache-folder t))

;;; Store additional config in a 'lisp' subfolder and add it to the load path so
;;; that `require' can find the files.
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

(when (require 'package nil t)
  ;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
  (setq package-user-dir (concat emacs-cache-folder "elpa"))
  (package-initialize))

;;; Site Lisp folder for local packages and development.
;; We need to roll it out manually since we want it first in the `load-path',
;; while `normal-top-level-add-subdirs-to-load-path' appends it to the very end.
(defun package-refresh-load-path (path)
  "Add every non-hidden sub-folder of PATH to `load-path'."
  (when (file-directory-p path)
    (dolist (dir (directory-files path t "^[^\\.]"))
      (when (file-directory-p dir)
        (setq load-path (add-to-list 'load-path dir))))))
(let ((site-lisp (expand-file-name "site-lisp/" user-emacs-directory)))
  (add-to-list 'load-path site-lisp)
  (package-refresh-load-path site-lisp))

;;; Local config.  See below for an example usage.
(load "local-before" t)

(require 'functions)
(require 'main)
(require 'visual)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Assembly
(push 'nasm-mode package-selected-packages)

;;; Asymptote
(add-to-list 'load-path "/usr/share/asymptote")
(autoload 'asy-mode "asy-mode" "Asymptote major mode." t)
(autoload 'lasy-mode "asy-mode" "Hybrid Asymptote/Latex major mode." t)
(autoload 'asy-insinuate-latex "asy-mode" "Asymptote insinuate LaTeX." t)
(add-to-list 'auto-mode-alist '("\\.asy$" . asy-mode))

;;; BBCode
(nconc package-selected-packages '(bbcode-mode))
(with-eval-after-load 'bbcode-mode (require 'init-bbcode))

;;; Bibtex
(setq bibtex-entry-format '(opts-or-alts required-fields numerical-fields whitespace realign last-comma delimiters braces sort-fields))
(setq bibtex-field-delimiters 'double-quotes)
(add-hook 'bibtex-mode-hook 'turn-off-indent-tabs)

;;; Bison/Flex
;; (nconc package-selected-packages '(bison-mode))

;;; Camcorder
(nconc package-selected-packages '(camcorder))
(with-eval-after-load 'camcorder
  (setq camcorder-gif-output-directory "~/temp"
        camcorder-output-directory "~/temp"))

;;; C/C++
(with-eval-after-load 'cc-mode (require 'init-cc))

;;; ChangeLog
(defun change-log-set-indent-rules ()
  (setq tab-width 2 left-margin 2))
(add-hook 'change-log-mode-hook 'change-log-set-indent-rules)

;;; Completion
(nconc package-selected-packages '(company helm-company))
(when (require 'company nil t)
  (setq company-idle-delay nil))

;;; Debbugs
(nconc package-selected-packages '(debbugs))
(with-eval-after-load 'debbugs
  (setq debbugs-gnu-all-severities t
        debbugs-gnu-persistency-file (expand-file-name "debbugs" emacs-cache-folder)))

;;; Diff
;;; REVIEW: Show permissions in ztree.
;;; See https://github.com/fourier/ztree/issues/50.
;;; TODO: In diff-mode, both `[[` and `C-M-a` do not go back to previous index
;;; once they are at the beginning of an index.
(nconc package-selected-packages '(ztree))

;;; Dired
;;; Dired is loaded after init.el, so configure it only then.
;;; TODO: Improve dired-du:
;;; - Hangs when the `ls` time format is changed.
;;; - Cache recursive results.
(nconc package-selected-packages '(dired-du))
(with-eval-after-load 'dired (require 'init-dired))

;;; Emms
(nconc package-selected-packages '(emms helm-emms emms-player-mpv))
(with-eval-after-load 'emms (require 'init-emms))

;;; Evil
(nconc package-selected-packages '(evil
                                   evil-commentary evil-mc evil-mc-extras linum-relative
                                   evil-ediff evil-magit evil-mu4e evil-org evil-collection))
(setq evil-want-integration nil)
(when (require 'evil nil t) (require 'init-evil))

;;; Eshell
;;; Extend completion.
(nconc package-selected-packages '(fish-completion bash-completion
                                                   pcomplete-extension pcmpl-args pcmpl-git))
(nconc package-selected-packages '(esh-autosuggest))
(when (and (executable-find "fish")
           (require 'fish-completion nil t))
  (setq fish-completion-fallback-on-bash-p t)
  (global-fish-completion-mode))
(with-eval-after-load 'eshell (require 'init-eshell))
(autoload 'eshell-or-new-session "eshell")

;;; GLSL
(nconc package-selected-packages '(glsl-mode))

;;; Go
(nconc package-selected-packages '(go-mode go-eldoc go-guru go-rename helm-go-package company-go))
(with-eval-after-load 'go-mode (require 'init-go))

;;; Graphviz dot
;;; The view command is broken but the preview command works (it displays the PNG
;;; in a new window), which is enough and probably not worth a fix.
(nconc package-selected-packages '(graphviz-dot-mode))

;;; GUD (GDB, etc.)
(with-eval-after-load 'gud (require 'init-gud))

;;; Helm
(nconc package-selected-packages '(helm helm-descbinds helm-ls-git))
(when (require 'helm-config nil t) (require 'init-helm))

;;; Hex editing
(nconc package-selected-packages '(nhexl-mode))

;;; Image
;;; TODO: Disable white frame.
;;; I think it's the cursor.
;;; Evil-mode reverts cursor changes.
;;; TODO: Implement other sxiv features:
;;; - Gamma
;;; - Marks
;;; - Gallery
;;; TODO: Is it possible to display an image fullscreen?
;;; TODO: Image+: Do no auto-adjust animated files
;;; https://github.com/mhayashi1120/Emacs-imagex/issues/10
;;; TODO: Image+: Restore animation state
;;; https://github.com/mhayashi1120/Emacs-imagex/issues/9
(nconc package-selected-packages '(image+))
(with-eval-after-load 'image
  (setq image-animate-loop t)
  (add-hook 'image-mode-hook 'image-toggle-animation)
  (require 'image+ nil t))

;;; Indentation engine fix.
;; (require 'smiext "init-smiext")

;;; Indentation style guessing.
;; (nconc 'package-selected-packages '(dtrt-indent))

;;; JavaScript
(add-hook 'js-mode-hook (lambda () (defvaralias 'js-indent-level 'tab-width)))

;;; Lisp
(dolist (hook '(lisp-mode-hook emacs-lisp-mode-hook))
  (add-hook hook 'turn-on-fmt-before-save)
  (add-hook hook 'turn-on-complete-filename)
  (add-hook hook 'turn-on-tab-width-to-8) ; Because some existing code uses tabs.
  (add-hook hook 'turn-off-indent-tabs)) ; Should not use tabs.
(define-key lisp-mode-shared-map (kbd "M-.") 'find-symbol-at-point)

;;; Common LISP
(setq inferior-lisp-program "clisp")

;;; Lua
(nconc package-selected-packages '(lua-mode))
(with-eval-after-load 'lua-mode (require 'init-lua))

;;; Magit
;;; Magit can be loaded just-in-time.
(nconc package-selected-packages '(magit))
(with-eval-after-load 'magit
  (setq auto-revert-mode-text "")
  (set-face-foreground 'magit-branch-remote "orange red")
  (setq git-commit-summary-max-length fill-column)
  ;; Avoid conflict with WM.
  (define-key magit-mode-map (kbd "s-<tab>") nil)
  (setq magit-diff-refine-hunk 'all))
(when (fboundp 'magit-status)
  (global-set-key (kbd "C-x g") 'magit-status))

;;; Mail
;;; mu4e is usually site-local and not part of ELPA.
(when (delq nil (mapcar (lambda (path) (string-match "/mu4e/\\|/mu4e$" path)) load-path))
  ;; (nconc package-selected-packages '(mu4e-maildirs-extension))
  (nconc package-selected-packages '(helm-mu)))
(with-eval-after-load 'mu4e (require 'init-mu4e))
(autoload 'mu4e-headers "mu4e")

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

;;; News
(nconc package-selected-packages '(elfeed))
(with-eval-after-load 'elfeed (require 'init-elfeed))

;;; Org-mode
(nconc package-selected-packages '(org-plus-contrib org-bullets)) ; org-plus contains latest Org mode.
(with-eval-after-load 'org (require 'init-org))
(autoload 'org-switch-agenda-file "org")
(autoload 'org-switch-agenda-file-other-window "org")

;;; Packaging
(nconc package-selected-packages '(esup package-lint))

;;; PDF
;;; pdf-tools requires poppler built with cairo support.
;;; We cannot defer loading as `pdf-tools-install' is required for PDF
;;; association.
;;; REVIEW: `save-place' does not seem to work with pdf-tools.
;;; See https://github.com/politza/pdf-tools/issues/18.
;;; TODO: windmove fails when selecting text and then moving up/down.
;;; It only fails if in evil mode.
(nconc package-selected-packages '(pdf-tools))
(when (require 'pdf-tools nil t)
  ;; (setq pdf-view-midnight-colors '("#ffffff" . "#000000"))
  (setq pdf-view-midnight-colors '("#ff9900" . "#0a0a12" )) ; Amber
  (add-hook 'pdf-view-mode-hook 'pdf-view-midnight-minor-mode)
  (pdf-tools-install t t t))

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

;;; Rainbow-mode
(nconc package-selected-packages '(rainbow-mode))
(when (require 'rainbow-mode nil t)
  (dolist (hook '(css-mode-hook html-mode-hook sass-mode-hook))
    (add-hook hook 'rainbow-mode)))

;;; Roff / Nroff
(with-eval-after-load 'nroff-mode (require 'init-nroff))

;;; Shell
(with-eval-after-load 'sh-script (require 'init-sh))
;;; Arch Linux PKGBUILD
(add-to-list 'auto-mode-alist '("PKGBUILD" . sh-mode))
;;; Gentoo
(add-to-list 'auto-mode-alist '("\\.ebuild\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.eclass\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("package\\.accept_keywords" . sh-mode))
(add-to-list 'auto-mode-alist '("package\\.mask" . sh-mode))
(add-to-list 'auto-mode-alist '("package\\.use" . sh-mode))
;;; If we ever need to edit exotic shell configs:
;; (nconc package-selected-packages '(fish-mode rc-mode))

;;; Srt (subtitles)
(add-to-list 'auto-mode-alist '("\\.srt\\'" . text-mode))

;;; StackExchange
(nconc package-selected-packages '(sx))
(with-eval-after-load 'sx
  (setq sx-cache-directory (concat emacs-cache-folder "sx")))

;;; Syntax checking
(nconc package-selected-packages '(flycheck helm-flycheck))
(when (require 'flycheck nil t) (require 'init-flycheck))

;;; System packages
(nconc package-selected-packages '(helm-system-packages))
(global-set-key (kbd "C-x c #") 'helm-system-packages)

;;; Terminal
(with-eval-after-load 'term
  ;; (require 'init-term)
  (setq term-buffer-maximum-size 0))

;;; TeX / LaTeX / Texinfo
(nconc package-selected-packages '(auctex latex-math-preview))
(with-eval-after-load 'tex (require 'init-tex))
;; LaTeX is defined in the same file as TeX.  To separate the loading, we add it
;; to the hook.
(add-hook 'latex-mode-hook (lambda () (require 'init-latex)))

;;; Torrent
(nconc package-selected-packages '(transmission))
(with-eval-after-load 'transmission
  ;; `transmission' will fail to start and will not run any hook if the daemon
  ;; is not up yet.
  ;; We need to advice the function :before to guarantee it starts.
  (defun transmission-start-daemon ()
    (unless (member "transmission-da"
                    (mapcar
                     (lambda (pid) (alist-get 'comm (process-attributes pid)))
                     (list-system-processes)))
      (call-process "transmission-daemon")
      (sleep-for 1)))
  (advice-add 'transmission :before 'transmission-start-daemon)
  (setq transmission-refresh-modes '(transmission-mode transmission-files-mode transmission-info-mode transmission-peers-mode)
        transmission-refresh-interval 1))

;;; Translator
;;; TODO: Find alternative package.
(autoload 'itranslate "package-itranslate" nil t)
(autoload 'itranslate-lines "package-itranslate" nil t)
(autoload 'itranslate-region "package-itranslate" nil t)

;;; Web forms.
;;; Remove auto-fill in web edits because wikis and forums do not like it.
;;; This works for qutebrowser, but may need changes for other browsers.
(defun browser-edit ()
  (when (require 'with-editor nil t) (with-editor-mode))
  (text-mode)
  (auto-fill-mode -1))
(add-to-list 'auto-mode-alist `(,(concat (getenv "BROWSER") "-editor-*") . browser-edit))

;;; Wgrep
(nconc package-selected-packages '(wgrep-helm wgrep-pt))
(when (require 'wgrep nil t)
  ;; TODO: wgrep-face is not so pretty.
  (set-face-attribute 'wgrep-face nil :inherit 'ediff-current-diff-C :foreground 'unspecified :background 'unspecified :box nil))

;;; Window manager
(nconc package-selected-packages '(exwm helm-exwm))
(when (require 'exwm nil t) (require 'init-exwm))

;;; XML / SGML
(defun sgml-setup ()
  (setq sgml-xml-mode t)
  ;; (toggle-truncate-lines) ; This seems to slow down Emacs.
  (turn-off-auto-fill))
(add-hook 'sgml-mode-hook 'sgml-setup)
(with-eval-after-load 'nxml-mode
  (set-face-foreground 'nxml-element-local-name "gold1")
  (defvaralias 'nxml-child-indent 'tab-width))
;;; Because XML is hard to read.
(add-hook 'nxml-mode-hook 'turn-on-tab-width-to-4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Finalization

;;; Don't let `customize' clutter my config.
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

;;; Local config. You can use it to set system specific variables, such as the
;;; external web browser or the geographical coordinates:
;;
;; (setq calendar-latitude 20.2158)
;; (setq calendar-longitude 105.938)
(load "local-after" t)
