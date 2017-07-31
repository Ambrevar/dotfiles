;;; Evil+Image

;; TODO: Why can't I set image-mode default state to 'motion?
(evil-initial-state 'image-mode 'motion)
(add-to-list 'evil-motion-state-modes 'image-mode)

(evil-define-key 'motion image-mode-map
  "<retun>" 'image-toggle-animation
  "<space>" 'image-scroll-up
  "S-<space>" 'image-scroll-down
  "F" 'image-goto-frame
  "H" 'image-previous-frame
  "L" 'image-next-frame
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
  "a0" 'image-reset-speed
  "ar" 'image-reverse-speed
  "\C-c\C-c" 'image-toggle-display
  "DEL" 'image-scroll-down)

(when evil-want-C-u-scroll
  (evil-define-key 'motion image-mode-map
    "C-u" 'image-scroll-up))

(provide 'init-evil-image)
