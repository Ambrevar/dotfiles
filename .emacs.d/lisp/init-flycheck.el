;;; Flycheck

(defun ambrevar/flycheck-and-whitespace-mode ()
  "Toggle `flycheck-mode' and `whitespace-mode'."
  (interactive)
  (if (derived-mode-p 'text-mode)
      (flyspell-mode)
    (flycheck-mode 'toggle)
    (if flyspell-mode (flyspell-mode 0) (flyspell-prog-mode)))
  (whitespace-mode 'toggle))
(global-set-key (kbd "<f9>") 'ambrevar/flycheck-and-whitespace-mode)

(define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck)

(provide 'init-flycheck)
