;;; Evil+term

(evil-set-initial-state 'term-mode 'insert)

(defun evil-term-char-mode-entry-function ()
  (interactive)
  (when (and (= (point) (point-max)) (term-in-line-mode))
    (term-char-mode)))

(defun evil-term-char-mode-exit-function ()
  (interactive)
  (when (term-in-char-mode)
    (term-line-mode)))

(defun evil-term-setup ()
  (add-hook 'evil-insert-state-entry-hook 'evil-term-char-mode-entry-function nil t)
  (add-hook 'evil-insert-state-exit-hook 'evil-term-char-mode-exit-function nil t))
(add-hook 'term-mode-hook 'evil-term-setup)

(provide 'init-evil-term)
