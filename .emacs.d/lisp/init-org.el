;;; Org mode

;;; TODO: org-import should be able to parse "|" in CSV files.

(define-key org-mode-map (kbd "C-c C-a") 'org-agenda)

(setq
 ;; Move annoying babel folder.
 org-babel-temporary-directory (concat emacs-cache-folder "babel")
 ;; Disable line splitting on M-RET.
 org-M-RET-may-split-line '((default))
 org-insert-heading-respect-content t
 org-enforce-todo-dependencies t
 org-deadline-warning-days 7
 org-agenda-default-appointment-duration 60
 org-agenda-columns-add-appointments-to-effort-sum t
 org-ellipsis " […]"
 ;; Org-mode aligns text.
 indent-tabs-mode nil)

;;; Agendas.
(add-to-list 'org-agenda-files "~/personal/todo/todo.org")
(defun org-find-first-agenda ()
  (interactive)
  (find-file (car org-agenda-files)))

;;; Set PDF association in Org-mode (original is 'default).
(setcdr (assoc "\\.pdf\\'" org-file-apps) 'emacs)

;;; Hooks.
(dolist (fun '(turn-off-linum turn-off-indent-tabs turn-off-auto-fill))
  (add-hook 'org-mode-hook fun))

(when (require 'org-contacts nil t)
  (setq org-contacts-files '("~/personal/contacts/contacts.org")))

(provide 'init-org)
