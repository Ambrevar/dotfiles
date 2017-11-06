;;; Evil+Helm

(global-set-key (kbd "M-y") 'helm-show-kill-ring)

(define-keys helm-map
  "C-\\" 'helm-toggle-resplit-and-swap-windows ; Becauste C-t is taken by evil-mv.
  "M-\\" 'helm-toggle-resplit-and-swap-windows)

(define-key helm-generic-files-map (kbd "M-o") 'helm-ff-run-switch-other-window)
(define-key helm-buffer-map (kbd "M-o") 'helm-buffer-switch-other-window)
(define-key helm-buffer-map (kbd "M-d") 'helm-buffer-run-kill-persistent)
(define-key helm-moccur-map (kbd "M-o") 'helm-moccur-run-goto-line-ow)
(define-key helm-grep-map (kbd "M-o") 'helm-grep-run-other-window-action)
(define-key helm-map (kbd "C-/") 'helm-quit-and-find-file)

(dolist (map (list helm-find-files-map helm-read-file-map))
  (define-keys map
    "M-." 'helm-end-of-buffer
    "M-," 'helm-beginning-of-buffer
    "M-o" 'helm-ff-run-switch-other-window
    "C-/" 'helm-ff-run-find-sh-command))

(with-eval-after-load 'package-helm-exwm
  (define-keys helm-exwm-map "M-d" 'helm-buffer-run-kill-persistent)
  (define-keys helm-exwm-map "M-o" 'helm-buffer-switch-other-window))

;; `helm-mark-or-exchange-rect' is not needed with Evil.
(global-set-key (kbd "C-x C-x") 'helm-all-mark-rings)

(provide 'init-evil-helm)
