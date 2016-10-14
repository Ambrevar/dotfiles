;; SGML

;; Just because XML is ugly.
(add-hook-and-eval
 'sgml-mode-hook
 (lambda ()
   (setq sgml-xml-mode t)
   ;; This seems to slow down Emacs.
   ; (toggle-truncate-lines)
   (turn-off-auto-fill)))

(provide 'mode-sgml)
