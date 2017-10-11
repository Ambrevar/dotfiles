;;; Evil+outline

(evil-define-key 'motion outline-mode-map
  (kbd "TAB") 'outline-toggle-children)

(provide 'init-evil-outline)
