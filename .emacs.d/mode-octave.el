;; Set comments to be '%' to be matlab-compatible.
(add-hook
 'octave-mode-hook
 (lambda ()
   (set (make-local-variable 'comment-start) "% ")
   ))

(provide 'mode-octave)
