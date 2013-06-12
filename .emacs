;; -*- mode:emacs-lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs config
;; 2013-06-12
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

;; Cache folder is everything we do not want to track.
(setq emacs-cache-folder "~/.cache/emacs/")
(if
    (not (file-directory-p emacs-cache-folder))
    (make-directory emacs-cache-folder t))

;; Local plugin folder for quick install. All files in this folder will be
;; accessible to Emacs config.
(add-to-list 'load-path "~/.emacs.d/plugins")


(load "~/.emacs.d/functions" nil t)
(load "~/.emacs.d/main" nil t)
(load "~/.emacs.d/mode-c.el" nil t)
(load "~/.emacs.d/mode-mediawiki.el" nil t)
(load "~/.emacs.d/mode-python.el" nil t)
(load "~/.emacs.d/mode-tex.el" nil t)
(load "~/.emacs.d/mode-texinfo.el" nil t)
(load "~/.emacs.d/mode-yasnippet.el" nil t)
(load "~/.emacs.d/personal" nil t)
(load "~/.emacs.d/theme" nil t)

;; We need to put it at the end to make sure it doesn't get overriden by other
;; minor modes.
(my-keys-minor-mode 1)

;; End of file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
