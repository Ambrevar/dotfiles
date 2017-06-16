;; Nroff

(let ((map nroff-mode-map))
  (define-key map (kbd "C-c C-b") 'nroff-bold)
  (define-key map (kbd "C-c C-i") 'nroff-italic))

;; Skeletons
(define-skeleton nroff-bold "Bold text." nil "\\fB" @ _ "\\fR" @)
(define-skeleton nroff-italic "Italic text." nil "\\fI" @ _ "\\fR" @)

(provide 'mode-nroff)
