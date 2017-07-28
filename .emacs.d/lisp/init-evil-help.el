;;; Evil+help-mode

(evil-define-key 'motion help-mode-map
  (kbd "SPC") 'scroll-up-command
  "\C-f" 'scroll-up-command
  "\C-b" 'scroll-down-command
  (kbd "TAB") 'forward-button
  (kbd "<backtab>") 'backward-button
  "\C-o" 'help-go-back)

(provide 'init-evil-help)
