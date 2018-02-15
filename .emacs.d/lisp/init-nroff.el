;;; Nroff

(ambrevar/define-keys nroff-mode-map
                      "C-c C-b" 'ambrevar/nroff-bold
                      "C-c C-i" 'ambrevar/nroff-italic)

;;; Skeletons
(define-skeleton ambrevar/nroff-bold "Bold text." nil "\\fB" @ _ "\\fR" @)
(define-skeleton ambrevar/nroff-italic "Italic text." nil "\\fI" @ _ "\\fR" @)

(provide 'init-nroff)
