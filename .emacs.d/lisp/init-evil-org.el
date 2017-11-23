;;; Evil+Org

;;; org-evil is not as polished as of May 2017.
;;; See https://github.com/Somelauw/evil-org-mode/blob/master/doc/keythemes.org for inspiration.

(add-hook 'org-mode-hook 'evil-org-mode)
;; No need for 'insert, 'shift (I use custom definitions), 'todo 'heading.
(evil-org-set-key-theme '(navigation textobjects additional))

;; TODO: Setup evil-org-agenda when merged.
(require 'evil-org-agenda nil t)

;;; This won't get merged upstream.  See
;;; https://github.com/Somelauw/evil-org-mode/issues/25.
(defun evil-org-meta-return ()
  "Like `org-meta-return' but switch to insert mode."
  (interactive)
  (evil-insert 1)
  (org-meta-return))

(defun evil-org-shiftleft ()
  (interactive)
  ;; TODO: Use `org-at-heading-or-item-p' instead?
  (if (or (org-on-heading-p) (org-at-planning-p))
      (org-shiftleft)
    (evil-window-top)))

(defun evil-org-shiftright ()
  (interactive)
  (if (or (org-on-heading-p) (org-at-planning-p))
      (org-shiftright)
    (evil-window-bottom)))

(defun evil-org-shiftup ()
  (interactive)
  (if (or (org-on-heading-p) (org-at-planning-p))
      (org-shiftup)
    (evil-lookup)))

(defun evil-org-shiftdown ()
  (interactive)
  (if (or (org-on-heading-p) (org-at-planning-p))
      (org-shiftdown)
    (call-interactively 'evil-join)))

(evil-define-key 'normal evil-org-mode-map
  "H" 'evil-org-shiftleft
  "J" 'evil-org-shiftdown
  "K" 'evil-org-shiftup
  "L" 'evil-org-shiftright
  "^" 'org-up-element ; Evil-Magit-inspired. TODO: Suggest upstream.
  "<" 'org-up-element ; Custom
  ">" 'org-down-element ; Custom
  (kbd "M-<return>") 'evil-org-meta-return)

(provide 'init-evil-org)
