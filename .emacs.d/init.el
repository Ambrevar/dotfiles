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

(load "main" nil t)
(load "theme" nil t)

(load "functions" nil t)
(load "personal" nil t)
;; (load "snippets" nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modes config

(add-hook 'c-mode-hook (lambda () (require 'mode-cc)))
(add-hook 'c++-mode-hook (lambda () (require 'mode-cc)))
(add-hook 'dot-mode-hook (lambda () (require 'mode-dot)))
(add-hook 'perl-mode-hook (lambda () (require 'mode-perl)))
(add-hook 'python-mode-hook (lambda () (require 'mode-python)))
(add-hook 'shell-mode-hook (lambda () (require 'mode-shell)))
(add-hook 'tex-mode-hook (lambda () (require 'mode-tex)))
(add-hook 'texinfo-mode-hook (lambda () (require 'mode-texinfo)))

(add-hook 'org-mode-hook (lambda () (require 'mode-org)))
(add-hook 'ediff-mode-hook (lambda () (require 'mode-ediff)))
(add-hook 'octave-mode-hook (lambda () (require 'mode-octave)))
(add-hook 'dired-mode-hook (lambda () (require 'mode-dired)))
(add-hook 'gud-mode-hook (lambda () (require 'mode-gud)))

(add-hook 'eshell-mode-hook (lambda () (require 'eshell-markdown)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extra modes

(require 'go-mode-load nil t)

;; Lua
(when (require 'lua-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode)))

(when (require 'markdown-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (set (make-local-variable 'paragraph-start) "
"))

;; .po support. This mode has no hooks.
(when (require 'po-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.po\\'\\|\\.po\\." . po-mode)))
(when (require 'po-find-file-coding-system nil t)
  (modify-coding-system-alist 'file "\\.po\\'\\|\\.po\\." 'po-find-file-coding-system))

;; Bison/flex
(when (require 'bison-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.yy?\\'" . bison-mode)))
(when (require 'flex-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.l\\'" . flex-mode)))

;; GLSL
(when (require 'glsl-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; We need to put it at the end to make sure it doesn't get overriden by other
;; minor modes.
(my-keys-minor-mode 1)

;; End of file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
