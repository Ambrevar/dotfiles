;; Evil+Eshell

(defun evil/eshell-next-prompt ()
  (when (get-text-property (point) 'read-only)
    ;; If at end of prompt, `eshell-next-prompt' will not move, so go backward.
    (beginning-of-line)
    (eshell-next-prompt 1)))

(defun evil/eshell-setup ()
  (dolist (hook '(evil-replace-state-entry-hook evil-insert-state-entry-hook))
    (add-hook hook 'evil/eshell-next-prompt nil t)))

(add-hook 'eshell-mode-hook 'evil/eshell-setup)

(defun evil/eshell-interrupt-process ()
  (interactive)
  (eshell-interrupt-process)
  (evil-insert 1))

;;; `eshell-mode-map' is reset when Eshell is initialized in `eshell-mode'. We
;;; need to add bindings to `eshell-first-time-mode-hook'.
(defun evil/eshell-set-keys ()
  (with-eval-after-load 'init-helm
    (evil-define-key 'insert eshell-mode-map "\C-e" 'helm-find-files))
  (evil-define-key 'normal eshell-mode-map
    "[" 'eshell-previous-prompt
    "]" 'eshell-next-prompt
    "\M-k" 'eshell-previous-prompt
    "\M-j" 'eshell-next-prompt
    "0" 'eshell-bol
    (kbd "RET") 'eshell-send-input
    (kbd "C-c C-c") 'evil/eshell-interrupt-process
    "\M-h" 'eshell-backward-argument
    "\M-l" 'eshell-forward-argument)
  (evil-define-key 'insert
    eshell-mode-map "\M-h" 'eshell-backward-argument
    "\M-l" 'eshell-forward-argument))

(add-hook 'eshell-first-time-mode-hook 'evil/eshell-set-keys)

(provide 'init-evil-eshell)
