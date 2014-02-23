;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;; accessible to Emacs config.
(add-to-list 'load-path "~/.emacs.d/plugins")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load main config

(load "functions" nil t)
(load "main" nil t)
(load "theme" nil t)
(load "personal" nil t)
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

;; Minor modes
(add-hook 'dired-mode-hook  (lambda () (require 'mode-dired)))
(add-hook 'ediff-mode-hook  (lambda () (require 'mode-ediff)))
(add-hook 'eshell-mode-hook (lambda () (require 'mode-eshell)))
(add-hook 'gud-mode-hook    (lambda () (require 'mode-gud)))
(add-hook 'octave-mode-hook (lambda () (require 'mode-octave)))
(add-hook 'org-mode-hook    (lambda () (require 'mode-org)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Third-party modes

;; TODO: implement function for loading and add support for fallback mode.

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
;; Tools

(autoload 'translate "tool-translate" nil t)
(autoload 'translate-line-by-line "tool-translate" nil t)

(autoload 'pdf-view "tool-pdf" nil t)
(autoload 'pdf-compress "tool-pdf" nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; We need to put it at the end to make sure it doesn't get overriden by other
;; minor modes.
(my-keys-minor-mode 1)

;; End of file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
