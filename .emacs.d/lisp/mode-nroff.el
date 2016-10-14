;; Nroff

(add-hook-and-eval
 'latex-mode-hook
 (lambda ()
   (local-set-key (kbd "C-c C-b") 'nroff-bold)
   (local-set-key (kbd "C-c C-i") 'nroff-italic)))

;; Skeletons
(define-skeleton nroff-bold "Bold text." nil "\\fB" @ _ "\\fR" @)
(define-skeleton nroff-italic "Italic text." nil "\\fI" @ _ "\\fR" @)

(provide 'mode-nroff)
