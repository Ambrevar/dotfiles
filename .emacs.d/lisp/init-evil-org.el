;;; Evil+Org

;;; org-evil is not as polished as of May 2017.
;;; See https://github.com/Somelauw/evil-org-mode/blob/master/doc/keythemes.org for inspiration.

(add-hook 'org-mode-hook 'evil-org-mode)
;; No need for 'insert, 'shift (I use custom definitions), 'todo 'heading.
(evil-org-set-key-theme '(navigation textobjects additional shift))

(defun evil-org-meta-return ()
  "Like `org-meta-return' but switch to insert mode."
  (interactive)
  (evil-insert 1)
  (org-meta-return))

(evil-define-key 'normal evil-org-mode-map
  "^" 'org-up-element ; Evil-Magit-inspired. TODO: Suggest upstream.
  "<" 'org-up-element ; Custom
  ">" 'org-down-element ; Custom
  (kbd "M-<return>") 'evil-org-meta-return)

(provide 'init-evil-org)
