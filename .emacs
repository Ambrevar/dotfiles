;; -*- mode:emacs-lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs config
;; 2013-01-09
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (add-to-list 'load-path "~/.emacs.d/")

;; Main should be loaded first.
(load "~/.emacs.d/main" nil t)

(load "~/.emacs.d/functions" nil t)
(load "~/.emacs.d/modes" nil t)
(load "~/.emacs.d/plugins" nil t)
(load "~/.emacs.d/theme" nil t)

;; End of file

