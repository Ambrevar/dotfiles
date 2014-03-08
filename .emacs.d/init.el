;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global variables.

(defvar my-keys-minor-mode-map (make-keymap)
  "Keymap for my-keys-minor-mode. See its docstring for more
details.")

(define-minor-mode my-keys-minor-mode
  "A minor mode so that all bindings assingned on the
my-keys-minor-mode-map override undesired major modes
bindings. We use a minor mode to override global keys. This is
also rather useful to list all personal global bindings: just
rgrep `my-keys-minor-mode-map' over `~/.emacs.d'.

Example: to assign some-function to C-i, use

  (define-key my-keys-minor-mode-map (kbd \"C-i\") 'some-function)"
  t " my-keys" 'my-keys-minor-mode-map)
(add-hook 'minibuffer-setup-hook (lambda () (my-keys-minor-mode 0) ) )

(defvar emacs-cache-folder "~/.cache/emacs/"
  "Cache folder is everything we do not want to track along with
  the configuration files.")
(if (not (file-directory-p emacs-cache-folder))
    (make-directory emacs-cache-folder t))

;; Load config easily.
(add-to-list 'load-path "~/.emacs.d")

;; Local plugin folder for quick install. All files in this folder will be
;; accessible to Emacs config. This is done to separate the versioned config
;; files from the external packages. For instance you can put package.el in
;; there for Emacs <24.
(add-to-list 'load-path "~/.emacs.d/local")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load main config

(require 'functions nil t)
(require 'main nil t)
(require 'theme nil t)
(require 'personal nil t)
;; (load "snippets" nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vanilla modes

;; Major modes
(add-hook 'c++-mode-hook     (lambda () (require 'mode-cc)))
(add-hook 'c-mode-hook       (lambda () (require 'mode-cc)))
(add-hook 'latex-mode-hook   (lambda () (require 'mode-latex)))
(add-hook 'perl-mode-hook    (lambda () (require 'mode-perl)))
(add-hook 'python-mode-hook  (lambda () (require 'mode-python)))
(add-hook 'sh-mode-hook      (lambda () (require 'mode-sh)))
(add-hook 'tex-mode-hook     (lambda () (require 'mode-tex)))
(add-hook 'texinfo-mode-hook (lambda () (require 'mode-texinfo)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Major modes pre-loading config.

;; sh-shell-file is initialized to the environment variable SHELL when sh-mode
;; is started with file without extension nor shabang. Since this the hook is
;; executed afterward, changing sh-shell-file in it will not alter the first
;; opened file.
(setq sh-shell-file "/bin/sh")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Minor modes
(add-hook 'dired-mode-hook  (lambda () (require 'mode-dired)))
(add-hook 'ediff-mode-hook  (lambda () (require 'mode-ediff)))
(add-hook 'eshell-mode-hook (lambda () (require 'mode-eshell)))
(add-hook 'gud-mode-hook    (lambda () (require 'mode-gud)))
(add-hook 'octave-mode-hook (lambda () (require 'mode-octave)))
(add-hook 'org-mode-hook    (lambda () (require 'mode-org)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Third-party modes

(when (require 'package nil t)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
  (package-initialize))

;; TODO: test this!
(defun load-external-mode (ext package &optional mode default)
  "Add EXT to auto-mode-alist such that it loads the associated
PACKAGE. EXT should be a regex. If PACKAGE has not the same name
as the mode, you should provide the real mode name in MODE. If
MODE is nil or unspecified, PACKAGE is used as the mode name.\n
We use an 'autoload to make the mode accessible interactively. We
need the 'require to check if package is loadable. It allows us
to fallback to the DEFAULT mode if provided."
  (let ((local-mode (if mode mode (string-to-symbol package))))
    (autoload local-mode package nil t)
    (add-to-list 'auto-mode-alist
                 (cons ext
                       `(lambda ()
                         (if (require ,package nil t)
                             (funcall ,local-mode)
                           ,(unless (null default) '(funcall default))
                           (error "Could not load %s" package)))))))

(autoload 'bison-mode "bison-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.yy?\\'" . bison-mode))
(autoload 'flex-mode "flex-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.l\\'" . flex-mode))

(autoload 'glsl-mode "glsl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.vert\\'\\|\\.frag\\'\\|\\.glsl\\'" . glsl-mode))

(autoload 'go-mode "go-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

;; Note that graphviz-mode has no 'provide'.
(autoload 'graphviz-dot-mode "graphviz-dot-mode" "Dot mode." t)
(add-to-list 'auto-mode-alist '("\\.dot\\'" . graphviz-dot-mode))
(add-hook 'graphviz-dot-mode-hook (lambda () (require 'mode-dot)))

(autoload 'lua-mode "lua-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))

(autoload 'markdown-mode "markdown-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.md\\'\\|\\.markdown\\'" . markdown-mode))
;; If we need more option, add it to a dedicated file.
(add-hook 'markdown-mode-hook (lambda () (set (make-local-variable 'paragraph-start) "
")))

(autoload 'mediawiki-mode "mediawiki-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.wiki\\'" . mediawiki-mode))
(add-hook 'mediawiki-mode-hook (lambda () (require 'mode-mediawiki)))

;; .po support. This mode has no hooks.
(autoload 'po-mode "po-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.po\\'\\|\\.po\\." . po-mode))
(when (fboundp 'po-find-file-coding-system)
  (modify-coding-system-alist 'file "\\.po\\'\\|\\.po\\." 'po-find-file-coding-system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Third-party tools
(autoload 'guess-style-set-variable "guess-style" nil t)
(autoload 'guess-style-guess-variable "guess-style")
(autoload 'guess-style-guess-all "guess-style" nil t)
(setq guess-style-info-mode 1)

(autoload 'pdf-view "tool-pdf" nil t)
(autoload 'pdf-compress "tool-pdf" nil t)

(autoload 'itranslate "tool-itranslate" nil t)
(autoload 'itranslate-lines "tool-itranslate" nil t)
(autoload 'itranslate-region "tool-itranslate" nil t)

(when (require 'multiple-cursors nil t)
  (setq mc/list-file (concat emacs-cache-folder "mc-lists.el"))
  ;; Load the file at the new location.
  (load mc/list-file t)
  (global-unset-key (kbd "C-<down-mouse-1>"))
  (define-key my-keys-minor-mode-map (kbd "C-<mouse-1>") 'mc/add-cursor-on-click)
  (define-key my-keys-minor-mode-map (kbd "C-x M-r") 'mc/edit-lines)
  (define-key my-keys-minor-mode-map (kbd "C-x M-m") 'mc/mark-more-like-this-extended)
  (define-key my-keys-minor-mode-map (kbd "C-x M-l") 'mc/mark-all-like-this-dwim))

(when (require 'xclip nil t)
  (turn-on-xclip))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; We need to put it at the end to make sure it doesn't get overriden by other
;; minor modes.
(my-keys-minor-mode 1)

;; End of file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
