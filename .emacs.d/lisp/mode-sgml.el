;; SGML

(add-hook-and-eval
 'sgml-mode-hook
 (lambda ()
   (setq sgml-xml-mode t)
   ; (toggle-truncate-lines) ; This seems to slow down Emacs.
   (turn-off-auto-fill)))

(provide 'mode-sgml)
