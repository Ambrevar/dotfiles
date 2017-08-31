;;; Evil+man-mode

(evil-define-key 'motion Man-mode-map
  (kbd "SPC") 'scroll-up-command
  (kbd "TAB") 'forward-button
  (kbd "<backtab>") 'backward-button
  "\M-sf" 'Man-goto-section
  "q" 'Man-quit
  "gm" 'man
  "\C-j" 'Man-next-section
  "\C-k" 'Man-previous-section
  "gr" 'Man-follow-manual-reference
  "gs" 'Man-goto-see-also-section
  "u" 'Man-update-manpage
  "J" 'Man-next-manpage
  "K" 'Man-previous-manpage)

(provide 'init-evil-man)
