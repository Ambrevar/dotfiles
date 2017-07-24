;;; Evil+term

(evil-set-initial-state 'term-mode 'insert)

(defun evil-term-line-mode-and-normal ()
  (interactive)
  (term-line-mode)
  (evil-normal-state))

(defun evil-term-char-mode-and-insert ()
  (interactive)
  (term-char-mode)
  (evil-insert-state))

(evil-define-key 'insert term-raw-map
  [escape] 'evil-term-line-mode-and-normal)

(evil-define-key '(normal insert) term-mode-map
  "\C-c\C-k" 'evil-term-char-mode-and-insert)

(provide 'init-evil-term)
