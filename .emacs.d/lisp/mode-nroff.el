;; Nroff

(define-keys nroff-mode-map
  "C-c C-b" 'nroff-bold
  "C-c C-i" 'nroff-italic)

;; Skeletons
(define-skeleton nroff-bold "Bold text." nil "\\fB" @ _ "\\fR" @)
(define-skeleton nroff-italic "Italic text." nil "\\fI" @ _ "\\fR" @)

(provide 'mode-nroff)
