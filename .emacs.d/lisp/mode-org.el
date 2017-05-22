;; Org mode config.

(local-set-key (kbd "C-c C-a") 'org-agenda)

;; Move annoying babel folder. This move does not seem to work properly.
(setq org-babel-temporary-directory (concat emacs-cache-folder "babel"))
;; Disable line splitting on M-RET
(setq org-M-RET-may-split-line '((default)))

(setq org-deadline-warning-days 7)
(setq org-agenda-default-appointment-duration 60)
(setq org-agenda-columns-add-appointments-to-effort-sum t)

;; Agendas.
;; If you want to add other agendas in a local file, use the following code:
; (with-eval-after-load "org"
;   (add-to-list 'org-agenda-files "/path/to/agenda.org"))
(add-to-list 'org-agenda-files "~/personal/todo/todo.org")

(setq org-enforce-todo-dependencies t)
;; Set PDF association in Org-mode (was Evince by default).
(with-eval-after-load "org"
  (setq indent-tab-mode nil) ; Org-mode aligns text.
  (require 'tool-pdf)
  ;; Change .pdf association directly within the alist
  (setcdr (assoc "\\.pdf\\'" org-file-apps)
          (concat pdf-viewer " " (mapconcat 'identity pdf-viewer-args " "))))

(add-hook-and-eval
 'org-mode-hook
 (lambda ()
   (setq indent-tabs-mode nil)
   (auto-fill-mode -1)))

(provide 'mode-org)
