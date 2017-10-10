;;; Evil+custom-mode

(evil-set-initial-state 'Custom-mode 'motion)

(evil-define-key 'motion custom-mode-map
  (kbd "SPC") 'scroll-up-command
  (kbd "S-SPC") 'scroll-down-command
  (kbd "DEL") 'scroll-down-command
  (kbd "RET") 'Custom-newline
  (kbd "TAB") 'widget-forward
  (kbd "S-TAB") 'widget-backward
  (kbd "<backtab>") 'widget-backward
  "\M-sf" 'Man-goto-section
  "<" 'Custom-goto-parent
  "\C-j" 'widget-forward
  "\C-k" 'widget-backward
  "\M-j" 'widget-forward ; custom
  "\M-k" 'widget-backward ; custom
  "q" 'Custom-buffer-done)

(provide 'init-evil-custom)
