;; Org mode config.
;; Move annoying babel folder. This move does not seem to work properly.
(setq org-babel-temporary-directory (concat emacs-cache-folder "babel"))
;; Disable line splitting on M-RET
(setq org-M-RET-may-split-line '((default)))

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
   (auto-fill-mode -1)))

(provide 'mode-org)
