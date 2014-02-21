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
;; Major modes

(add-hook 'c++-mode-hook     (lambda () (require 'mode-cc)))
(add-hook 'c-mode-hook       (lambda () (require 'mode-cc)))
(add-hook 'latex-mode-hook   (lambda () (require 'mode-latex)))
(add-hook 'perl-mode-hook    (lambda () (require 'mode-perl)))
(add-hook 'python-mode-hook  (lambda () (require 'mode-python)))
(add-hook 'shell-mode-hook   (lambda () (require 'mode-shell)))
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
;; Extra modes

(when (require 'bison-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.yy?\\'" . bison-mode)))

(when (require 'flex-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.l\\'" . flex-mode)))

(when (require 'glsl-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode)))

(when (require 'go-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode)))

;; This mode has no 'provide'.
(when (autoload 'graphviz-dot-mode "graphviz-dot-mode" "Dot mode." t)
  (add-to-list 'auto-mode-alist '("\\.dot\\'" . graphviz-dot-mode))
  (add-hook 'dot-mode-hook (lambda () (require 'mode-dot))))

(when (require 'lua-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode)))

(when (require 'markdown-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  ;; If we need more option, add it to dedicated file.
  (set (make-local-variable 'paragraph-start) "
"))

(when (require 'mediawiki nil t)
  (add-to-list 'auto-mode-alist '("\\.wiki\\'" . mediawiki-mode))
  (add-hook 'mediawiki-mode-hook (lambda () (require 'mode-mediawiki))))

;; .po support. This mode has no hooks.
(when (require 'po-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.po\\'\\|\\.po\\." . po-mode)))
(when (require 'po-find-file-coding-system nil t)
  (modify-coding-system-alist 'file "\\.po\\'\\|\\.po\\." 'po-find-file-coding-system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; We need to put it at the end to make sure it doesn't get overriden by other
;; minor modes.
(my-keys-minor-mode 1)

(add-hook
 'after-init-hook
 (lambda ()
   (message "Init completed in %.2fms"
            (time-subtract-millis after-init-time before-init-time))))

;; End of file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
