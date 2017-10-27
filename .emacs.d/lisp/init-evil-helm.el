;;; Evil+Helm

(global-set-key (kbd "M-y") 'helm-show-kill-ring)

;;; To navigate helm entries with hjkl, using the C- modifier would conflict
;;; with C-h (help prefix) and C-k (`evil-insert-digraph').  We use M- instead.
;;; We cannot use normal mode to navigate if we also want to use it to edit the
;;; minibuffer content.
(define-keys helm-map
  "C-\\" 'helm-toggle-resplit-and-swap-windows
  "M-\\" 'helm-toggle-resplit-and-swap-windows
  "C-f" 'helm-next-page
  "C-b" 'helm-previous-page
  "M-h" 'helm-next-source
  "M-j" 'helm-next-line
  "M-k" 'helm-previous-line
  "M-l" 'helm-execute-persistent-action
  "M-." 'helm-end-of-buffer
  "M-," 'helm-beginning-of-buffer
  "<escape>" 'helm-keyboard-quit)

(evil-define-key 'normal helm-map
  "j" 'helm-next-line
  "k" 'helm-previous-line
  "g" 'helm-beginning-of-buffer
  "G" 'helm-end-of-buffer
  "SPC" 'helm-toggle-visible-mark
  (kbd "C-f") 'helm-next-page
  (kbd "C-b") 'helm-previous-page)

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
    "C-/" 'helm-ff-run-find-sh-command
    "M-h" 'helm-find-files-up-one-level
    "M-l" 'helm-execute-persistent-action
    "C-l" nil)) ; So the header displays the above binding.

(with-eval-after-load 'init-exwm
  (define-keys exwm/helm-browser-map "M-d" 'helm-buffer-run-kill-persistent)
  (define-keys exwm/helm-browser-map "M-o" 'helm-buffer-switch-other-window))

;; `helm-mark-or-exchange-rect' is not needed with Evil.
(global-set-key (kbd "C-x C-x") 'helm-all-mark-rings)

(provide 'init-evil-helm)
