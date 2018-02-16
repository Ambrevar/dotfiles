;;; Org mode

;;; TODO: org-import should be able to parse "|" in CSV files.

(define-key org-mode-map (kbd "C-c C-a") 'org-agenda)

(setq
 ;; Disable line splitting on M-RET.
 org-M-RET-may-split-line '((default))
 org-insert-heading-respect-content t
 org-enforce-todo-dependencies t
 org-deadline-warning-days 7
 org-agenda-default-appointment-duration 60
 org-agenda-columns-add-appointments-to-effort-sum t
 org-ellipsis " […]"
 org-adapt-indentation nil
 ;; Add keywords.
 org-todo-keywords '((sequence "TODO" "REVIEW" "DONE"))
 ;; org-todo-keyword-faces '(("REVIEW" :inherit org-done))
 ;; Priorities.
 org-priority-start-cycle-with-default nil
 org-default-priority 67
 ;; Org-mode aligns text.
 indent-tabs-mode nil)

;;; Agendas.
(add-to-list 'org-agenda-files "~/personal/todo/todo.org")
(defun ambrevar/org-switch-agenda-file (&optional other-window)
  "Switch between org-agenda and the first org-agenda-files."
  (interactive "P")
  (if (and buffer-file-name
           (member (expand-file-name buffer-file-name) (mapcar 'expand-file-name org-agenda-files)))
      (org-agenda)
    (let ((b (find-buffer-visiting (car org-agenda-files))))
      (if b
          (if (get-buffer-window b)
              (select-window (get-buffer-window b))
            (funcall (if other-window 'switch-to-buffer-other-window 'switch-to-buffer) b))
        (funcall (if other-window 'find-file-other-window 'find-file) (car org-agenda-files))))))

(defun ambrevar/org-switch-agenda-file-other-window ()
  "Like `ambrevar/org-switch-agenda-file' but use other window if possible."
  (interactive)
  (ambrevar/org-switch-agenda-file t))

;;; Set PDF association in Org-mode (original is 'default).
(setcdr (assoc "\\.pdf\\'" org-file-apps) 'emacs)

;;; Hooks.
(dolist (fun '(ambrevar/turn-off-linum ambrevar/turn-off-indent-tabs turn-off-auto-fill))
  (add-hook 'org-mode-hook fun))

(when (require 'org-contacts nil t)
  (let ((contacts "~/personal/contacts/contacts.org"))
    (when (file-exists-p contacts)
      ;; When used to auto-complete e-mail addresses, the file is automatically
      ;; loaded.  The buffer usually need not be restored by a desktop session.
      (when desktop-save-mode
        (setq desktop-files-not-to-save
              (concat (substring desktop-files-not-to-save 0 -2) "\\|" (regexp-quote (expand-file-name contacts)) "\\)")))
      (setq org-contacts-files (list contacts)))))

(when (require 'org-bullets nil t)
  (add-hook 'org-mode-hook 'org-bullets-mode))

(provide 'init-org)
