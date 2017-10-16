;;; Evil+Image+

(when (require 'image+ nil t)
  (evil-define-key 'motion image-mode-map
    "+" 'imagex-sticky-zoom-in
    "-" 'imagex-sticky-zoom-out
    "M" 'imagex-sticky-maximize
    "m" 'imagex-auto-adjust-mode
    "O" 'imagex-sticky-restore-original
    "S" 'imagex-sticky-save-image
    "r" 'imagex-sticky-rotate-right
    "l" 'imagex-sticky-rotate-left))

(provide 'init-evil-image+)
