;; -*- mode:emacs-lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs config
;; 2013-01-09
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/")

;; Main should be loaded first.
(load "main" nil t)

(load "theme" nil t)
(load "modes" nil t)
(load "functions" nil t)
;; (load "plugins" nil t)

;; End of file

