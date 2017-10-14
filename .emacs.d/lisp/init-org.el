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
 org-adapt-indentation nil
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
  (let ((contacts "~/personal/contacts/contacts.org"))
    (when (file-exists-p contacts)
      ;; When used to auto-complete e-mail addresses, the file is automatically
      ;; loaded.  The buffer usually need not be restored by a desktop session.
      (setq desktop-files-not-to-save (concat (substring desktop-files-not-to-save 0 -2) "\\|" (regexp-quote (expand-file-name contacts)) "\\)"))
      (setq org-contacts-files (list contacts)))))

;;; Add keywords.
(setq org-todo-keywords '((sequence "TODO" "REVIEW" "DONE")))
(setq org-todo-keyword-faces '(("REVIEW" :inherit org-done)))

;;; Priorities.
(setq org-priority-start-cycle-with-default nil
      org-default-priority 66)

(provide 'init-org)
