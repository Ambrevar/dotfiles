;; Org mode config.

;; Move annoying babel folder. This move does not seem to work properly.
(setq org-babel-temporary-directory (concat emacs-cache-folder "babel"))
;; Disable line splitting on M-RET
(setq org-M-RET-may-split-line '((default)))

(setq org-deadline-warning-days 7)
(setq org-agenda-default-appointment-duration 60)
(setq org-agenda-columns-add-appointments-to-effort-sum t)

(setq org-agenda-files '("~/todo.org"))
(setq org-enforce-todo-dependencies t)
;; Set PDF association in Org-mode (was Evince by default).
(eval-after-load "org"
  '(progn
     (require 'tool-pdf)
     ;; Change .pdf association directly within the alist
     (setcdr (assoc "\\.pdf\\'" org-file-apps)
             (concat pdf-viewer " " (mapconcat 'identity pdf-viewer-args " ")))))

(add-hook
 'org-mode-hook
 (lambda ()
   (local-set-key (kbd "C-c C-a") 'org-agenda)
   (auto-fill-mode -1)))

(provide 'mode-org)
