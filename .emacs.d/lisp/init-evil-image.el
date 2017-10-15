;;; Evil+Image

(evil-set-initial-state 'image-mode 'motion)

(evil-define-key 'motion image-mode-map
  (kbd "<return>") 'image-toggle-animation
  (kbd "<space>") 'image-scroll-up
  (kbd "S-<space>") 'image-scroll-down
  "F" 'image-goto-frame
  "," 'image-previous-frame ; mpv-style
  "." 'image-next-frame ; mpv-style
  "H" 'image-transform-fit-to-height
  "W" 'image-transform-fit-to-width
  "K" 'image-previous-file
  "J" 'image-next-file
  "q" 'quit-window
  "gg" 'image-bob
  "G" 'image-eob
  "h" 'image-backward-hscroll
  "l" 'image-forward-hscroll
  "0" 'image-bol
  "^" 'image-bol
  "$" 'image-eol
  "j" 'image-next-line
  "k" 'image-previous-line
  "\C-d" 'image-scroll-down
  "a+" 'image-increase-speed
  "a-" 'image-decrease-speed
  "}" 'image-increase-speed ; mpv-style
  "{" 'image-decrease-speed ; mpv-style
  "a0" 'image-reset-speed
  "ar" 'image-reverse-speed
  "\C-c\C-c" 'image-toggle-display
  (kbd "<delete>") 'image-scroll-down)

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

(when evil-want-C-u-scroll
  (evil-define-key 'motion image-mode-map
    (kbd "C-u") 'image-scroll-up))

(provide 'init-evil-image)
