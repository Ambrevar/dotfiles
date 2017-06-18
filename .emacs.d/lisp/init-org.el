;;; Org mode

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
 ;; Org-mode aligns text.
 indent-tab-mode nil)

;;; Agendas.
;;; If you want to add other agendas in a local file, use the following code:
;; (with-eval-after-load "org"
;;   (add-to-list 'org-agenda-files "/path/to/agenda.org"))
(add-to-list 'org-agenda-files "~/personal/todo/todo.org")

;;; Set PDF association in Org-mode (was Evince by default).
(require 'init-pdf)
(setcdr (assoc "\\.pdf\\'" org-file-apps)
        (concat pdf-viewer " " (mapconcat 'identity pdf-viewer-args " ")))

;;; Hooks.
(dolist (fun '(turn-off-linum turn-off-indent-tabs turn-off-auto-fill))
  (add-hook 'org-mode-hook fun))

(provide 'init-org)
