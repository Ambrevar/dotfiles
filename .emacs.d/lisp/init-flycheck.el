;;; Flycheck

(defun flycheck-or-whitespace-mode ()
  "Toggle `flycheck-mode' and `whitespace-mode'."
  (interactive)
  (when (derived-mode-p 'prog-mode)
    (flycheck-mode 'toggle))
  (whitespace-mode 'toggle))
(global-set-key (kbd "<f9>") 'flycheck-or-whitespace-mode)

(define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck)

(provide 'init-flycheck)
