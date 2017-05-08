;; God mode

;; We cannot use mickey if we want to use it from Helm.
(global-set-key (kbd "<escape>") 'god-local-mode)
(define-key god-local-mode-map (kbd ".") 'repeat)

(require 'god-mode-isearch)
(define-key isearch-mode-map (kbd "<escape>") 'god-mode-isearch-activate)
(define-key god-mode-isearch-map (kbd "<escape>") 'god-mode-isearch-disable)

(defun god-toggle-on-overwrite ()
  "Toggle god-mode on overwrite-mode."
  (if (bound-and-true-p overwrite-mode)
      (god-local-mode-pause)
    (god-local-mode-resume)))
(add-hook 'overwrite-mode-hook 'god-toggle-on-overwrite)

(defun my-update-cursor ()
  (setq cursor-type
        (if god-local-mode 'bar 'box)))
(add-hook 'god-mode-enabled-hook 'my-update-cursor)
(add-hook 'god-mode-disabled-hook 'my-update-cursor)

(add-to-list 'god-exempt-major-modes 'Info-mode)

(god-mode)

;; Extend general map for convenience.
(define-key mickey-minor-mode-map (kbd "C-x C-1") 'delete-other-windows)
(define-key mickey-minor-mode-map (kbd "C-x C-2") 'split-window-below)
(define-key mickey-minor-mode-map (kbd "C-x C-3") 'split-window-right)
(define-key mickey-minor-mode-map (kbd "C-x C-0") 'delete-window)
(define-key mickey-minor-mode-map (kbd "C-<") 'beginning-of-buffer)
(define-key mickey-minor-mode-map (kbd "C->") 'end-of-buffer)

(provide 'tool-god)
