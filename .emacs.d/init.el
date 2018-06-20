;;; Emacs config

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Prerequisites

(let ((minver "26.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))

;;; Speed up init.
;;; Temporarily reduce garbage collection during startup. Inspect `gcs-done'.
(defun ambrevar/reset-gc-cons-threshold ()
  (setq gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value))))
(setq gc-cons-threshold (* 64 1024 1024))
(add-hook 'after-init-hook 'ambrevar/reset-gc-cons-threshold)
;;; Temporarily disable the file name handler.
(setq default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(defun ambrevar/reset-file-name-handler-alist ()
  (setq file-name-handler-alist default-file-name-handler-alist))
(add-hook 'after-init-hook 'ambrevar/reset-file-name-handler-alist)

;;; Avoid the "loaded old bytecode instead of newer source" pitfall.
(setq load-prefer-newer t)

;;; Store additional config in a 'lisp' subfolder and add it to the load path so
;;; that `require' can find the files.
;;; This must be done before moving `user-emacs-directory'.
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

;;; Move user-emacs-directory so that user files don't mix with cache files.
(setq user-emacs-directory "~/.cache/emacs/")

(when (require 'package nil t)
  ;; TODO: MELPA's https sometimes return
  ;;   emacs melpa invalid: certificate host does not match hostname
  ;; Try the following:
  ;;   (setq tls-checktrust nil)
  ;; Different Emacs version have different byte code.  If a versioned ELPA
  ;; directory is found, use it.
  (let ((versioned-dir (format "elpa-%s.%s" emacs-major-version emacs-minor-version)))
    (when (member versioned-dir (directory-files (expand-file-name ".." package-user-dir)))
      (setq package-user-dir (expand-file-name (concat "../" versioned-dir) package-user-dir))))
  (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")))
  (add-to-list 'package-archives '("melpa" . "https://melpa.milkbox.net/packages/"))
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
  (package-initialize))

;;; Site Lisp folder for local packages and development.
;; We need to roll it out manually since we want it first in the `load-path',
;; while `normal-top-level-add-subdirs-to-load-path' appends it to the very end.
(defun ambrevar/package-refresh-load-path (path)
  "Add every non-hidden sub-folder of PATH to `load-path'."
  (when (file-directory-p path)
    (dolist (dir (directory-files path t "^[^\\.]"))
      (when (file-directory-p dir)
        (setq load-path (add-to-list 'load-path dir))
        (dolist (subdir (directory-files dir t "^[^\\.]"))
          (when (file-directory-p subdir)
            (setq load-path (add-to-list 'load-path subdir))))))))
(let ((site-lisp (expand-file-name "site-lisp/" "~/.local/share/emacs/")))
  (add-to-list 'load-path site-lisp)
  (ambrevar/package-refresh-load-path site-lisp))

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
(add-hook 'bibtex-mode-hook 'ambrevar/turn-off-indent-tabs)

;;; Bison/Flex
;; (nconc package-selected-packages '(bison-mode))

;;; C/C++
(with-eval-after-load 'cc-mode (require 'init-cc))

;;; ChangeLog
(defun ambrevar/change-log-set-indent-rules ()
  (setq tab-width 2 left-margin 2))
(add-hook 'change-log-mode-hook 'ambrevar/change-log-set-indent-rules)

;;; Completion
(nconc package-selected-packages '(company helm-company))
(when (require 'company nil t)
  (setq company-idle-delay nil))

;;; Debbugs
(nconc package-selected-packages '(debbugs))
(with-eval-after-load 'debbugs
  (setq debbugs-gnu-all-severities t))

;;; Diff
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

;;; Daemons.
(nconc package-selected-packages '(daemons))

;;; Emms
(nconc package-selected-packages '(emms helm-emms))
(with-eval-after-load 'emms (require 'init-emms))

;;; Engine
(nconc package-selected-packages '(engine-mode))
(when (require 'engine-mode nil t)
  (require 'init-engine))

;;; Evil
(nconc package-selected-packages '(evil
                                   evil-commentary evil-multiedit
                                   evil-magit evil-org evil-collection))
(setq evil-want-integration nil)
(when (require 'evil nil t) (require 'init-evil))

;;; Eshell
;;; Extend completion.
(nconc package-selected-packages '(fish-completion bash-completion
                                                   pcomplete-extension pcmpl-args pcmpl-git))
(nconc package-selected-packages '(esh-autosuggest))
(with-eval-after-load 'bash-completion
  ;; REVIEW: Upstream should set path dynamically.
  ;; https://github.com/szermatt/emacs-bash-completion/issues/33
  ;; Done.  Need to update Guix package.
  (setq bash-completion-prog (executable-find "bash")))
(when (and (executable-find "fish")
           (require 'fish-completion nil t))
  (setq fish-completion-fallback-on-bash-p t)
  (global-fish-completion-mode))
(with-eval-after-load 'eshell (require 'init-eshell))
(autoload 'ambrevar/eshell-or-new-session "eshell")

;;; Eww
(with-eval-after-load 'eww (require 'init-eww))
(autoload 'ambrevar/eww-switch-back "eww")

;;; Expand region.
(nconc package-selected-packages '(expand-region))
(when  (require 'expand-region nil t)
  (global-set-key (kbd "C-=") 'er/expand-region))

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

;;; Guix
(when (executable-find "guix")
  (nconc package-selected-packages '(guix))
  (defun ambrevar/init-guix ()
    (and buffer-file-name
         (string-match "\\<guix\\>" buffer-file-name)
         (guix-devel-mode)))
  (add-hook 'scheme-mode-hook 'ambrevar/init-guix))

;;; Helm
(nconc package-selected-packages '(helm helm-descbinds helm-ls-git))
(when (require 'helm-config nil t) (require 'init-helm))

;;; Hex editing
(nconc package-selected-packages '(nhexl-mode))

;;; Iedit
(nconc package-selected-packages '(iedit))
(when (require 'iedit nil t)
  (global-set-key (kbd "C-;") 'iedit-mode))

;;; Image
;;; TODO: Disable white frame.
;;; I think it's the cursor.
;;; Evil-mode reverts cursor changes.
;; (set-face-foreground 'cursor "black")
;;; TODO: Implement other sxiv features:
;;; - Gamma
;;; - Marks
;;; - Gallery
;;; TODO: Is it possible to display an image fullscreen?
;;; TODO: Image+: Dot no auto-adjust animated files
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
(nconc package-selected-packages '(lispy lispyville rainbow-delimiters geiser slime))
(with-eval-after-load 'lisp-mode (require 'init-lisp))

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
  ;; Customize what to fold by default.
  ;; (push (cons [* commitbuf] 'hide) magit-section-initial-visibility-alist)
  ;; Avoid conflict with WM.
  (define-key magit-mode-map (kbd "s-<tab>") nil)
  (setq magit-diff-refine-hunk 'all))
(when (fboundp 'magit-status)
  (global-set-key (kbd "C-x g") 'magit-status))

;;; Mail
;;; mu4e is usually site-local and not part of ELPA.
(when (or (fboundp 'mu4e)
          (delq nil (mapcar (lambda (path) (string-match "/mu4e/\\|/mu4e$" path)) load-path)))
  ;; (nconc package-selected-packages '(mu4e-maildirs-extension))
  (nconc package-selected-packages '(helm-mu)))
(with-eval-after-load 'mu4e
  ;; mu4e-conversation must be enabled here.
  ;; REVIEW: https://github.com/djcb/mu/issues/1258
  (when (require 'mu4e-conversation nil t)
    (setq mu4e-view-func 'mu4e-conversation)
    (setq mu4e-headers-show-threads nil
          mu4e-headers-include-related nil)
    ;; For testing purposes:
    ;; (set-face-background mu4e-conversation-sender-1 "#335533")
    ;; (set-face-background mu4e-conversation-sender-2 "#553333")
    ;; (set-face-background mu4e-conversation-sender-3 "#333355")
    ;; (set-face-background mu4e-conversation-sender-4 "#888855")
    (defun mu4e-conversation-toggle ()
      "Toggle-replace `mu4e-view' with `mu4e-conversation' everywhere."
      (interactive)
      (if (eq mu4e-view-func 'mu4e-conversation)
          (setq mu4e-view-func 'mu4e~headers-view-handler)
        (setq mu4e-view-func 'mu4e-conversation))))
  (require 'init-mu4e))
(autoload 'ambrevar/mu4e-headers "mu4e")

;;; Makefile
(with-eval-after-load 'make-mode (require 'init-makefile))

;;; Markdown
(nconc package-selected-packages '(markdown-mode))
(with-eval-after-load 'markdown-mode (require 'init-markdown))

;;; Matlab / Octave
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode)) ; matlab
(defun ambrevar/octave-set-comment-start ()
  "Set comment character to '%' to be Matlab-compatible."
  (set (make-local-variable 'comment-start) "% "))
(add-hook 'octave-mode-hook 'ambrevar/octave-set-comment-start)

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
(autoload 'ambrevar/elfeed-switch-back "elfeed")

;;; Org-mode
(nconc package-selected-packages '(org-plus-contrib org-bullets)) ; org-plus contains latest Org mode.
(with-eval-after-load 'org (require 'init-org))
(autoload 'ambrevar/org-switch-agenda-file "org")
(autoload 'ambrevar/org-switch-agenda-file-other-window "org")

;;; Packaging
(nconc package-selected-packages '(esup package-lint))

;;; Pass
(nconc package-selected-packages '(helm-pass))

;;; PDF
;;; pdf-tools requires poppler built with cairo support.
;;; We cannot defer loading as `pdf-tools-install' is required for PDF
;;; association.
;;; REVIEW: `save-place' does not seem to work with pdf-tools.
;;; See https://github.com/politza/pdf-tools/issues/18.
;;; TODO: windmove fails when selecting text and then moving up/down.
;;; It only fails in Evil mode.
(nconc package-selected-packages '(pdf-tools))
(when (require 'pdf-tools nil t)
  ;; (setq pdf-view-midnight-colors '("#ffffff" . "#000000"))
  (setq pdf-view-midnight-colors '("#ff9900" . "#0a0a12" )) ; Amber
  (add-hook 'pdf-view-mode-hook 'pdf-view-midnight-minor-mode)
  (pdf-tools-install t t t))

;;; Perl
(defun ambrevar/perl-set-indent-rules ()
  (defvaralias 'perl-indent-level 'tab-width))
(defun ambrevar/perl-set-compiler ()
  (setq compile-command (concat "perl " (shell-quote-argument buffer-file-name))))
(add-hook 'perl-mode-hook 'ambrevar/perl-set-indent-rules)
(add-hook 'perl-mode-hook 'ambrevar/perl-set-compiler)

;;; Pinentry
(nconc package-selected-packages '(pinentry)) ; pinentry.el was remombed from Emacs 26.

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

;;; Screencast
;; (nconc package-selected-packages '(camcorder))
(with-eval-after-load 'camcorder
  (setq camcorder-output-directory (expand-file-name "temp" "~")
        camcorder-gif-output-directory camcorder-output-directory)
  (setq camcorder-recording-command '("recordmydesktop" " --fps 10 --no-sound --windowid " window-id " -o " file))
  (add-to-list 'camcorder-gif-conversion-commands '("ffmpeg-slow" "ffmpeg -i " input-file " -vf 'fps=10,scale=1024:-1:flags=lanczos' " gif-file)))
(nconc package-selected-packages '(gif-screencast keycast))
(with-eval-after-load 'gif-screencast
  (define-key gif-screencast-mode-map (kbd "<f8>") 'gif-screencast-toggle-pause)
  (define-key gif-screencast-mode-map (kbd "<f9>") 'gif-screencast-stop))

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

(nconc package-selected-packages '(strace-mode))

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
  (defun ambrevar/transmission-start-daemon ()
    (unless (member "transmission-da"
                    (mapcar
                     (lambda (pid) (alist-get 'comm (process-attributes pid)))
                     (list-system-processes)))
      (call-process "transmission-daemon")
      (sleep-for 1)))
  (advice-add 'transmission :before 'ambrevar/transmission-start-daemon)
  (setq transmission-refresh-modes '(transmission-mode transmission-files-mode transmission-info-mode transmission-peers-mode)
        transmission-refresh-interval 1))

;;; Translator
(nconc package-selected-packages '(google-translate))
(when (require 'google-translate nil t)
  (require 'google-translate-default-ui)
  ;; (global-set-key "\C-ct" 'google-translate-at-point)
  ;; (global-set-key "\C-cT" 'google-translate-query-translate)
  (defun ambrevar/google-translate-line ()
    "Translate current line and insert result after it, separated by ' = '."
    (interactive)
    (let* ((langs (google-translate-read-args nil nil))
           (source-language (car langs))
           (target-language (cadr langs))
           text
           result)
      (end-of-line)
      (just-one-space)
      (setq text (buffer-substring-no-properties
                  (line-beginning-position) (line-end-position)))
      (setq result (with-temp-buffer
                     (google-translate-translate
                      source-language target-language
                      text
                      'current-buffer)
                     (buffer-string))
            (insert "= " result)))))

;;; Web forms.
;;; Remove auto-fill in web edits because wikis and forums do not like it.
;;; This works for qutebrowser, but may need changes for other browsers.
(defun ambrevar/browser-edit ()
  (when (require 'with-editor nil t) (with-editor-mode))
  (text-mode)
  (auto-fill-mode -1))
(add-to-list 'auto-mode-alist `(,(concat (getenv "BROWSER") "-editor-*") . ambrevar/browser-edit))

;;; Wgrep
(nconc package-selected-packages '(wgrep-helm wgrep-pt))
(when (require 'wgrep nil t)
  ;; TODO: wgrep-face is not so pretty.
  (set-face-attribute 'wgrep-face nil :inherit 'ediff-current-diff-C :foreground 'unspecified :background 'unspecified :box nil))

;;; Window manager
(nconc package-selected-packages '(exwm helm-exwm))
(nconc package-selected-packages '(pulseaudio-control))
(with-eval-after-load 'pulseaudio-control
  ;; REVIEW: Upstream should set path dynamically.
  ;; https://github.com/flexibeast/pulseaudio-control/issues/7
  (setq pulseaudio-control-pactl-path (executable-find "pactl")))
(when (require 'exwm nil t) (require 'init-exwm))

;;; XML / SGML
(defun ambrevar/sgml-setup ()
  (setq sgml-xml-mode t)
  ;; (toggle-truncate-lines) ; This seems to slow down Emacs.
  (turn-off-auto-fill))
(add-hook 'sgml-mode-hook 'ambrevar/sgml-setup)
(with-eval-after-load 'nxml-mode
  (set-face-foreground 'nxml-element-local-name "gold1")
  (defvaralias 'nxml-child-indent 'tab-width))
;;; Because XML is hard to read.
(add-hook 'nxml-mode-hook 'ambrevar/turn-on-tab-width-to-4)

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
