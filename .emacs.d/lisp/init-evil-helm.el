;;; Evil+Helm

;; To navigate helm entries with hjkl, using the C- modifier would conflict
;; with C-h (help prefix) and C-k (`evil-insert-digraph').  We use M- instead.
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
  "g" 'helm-beginning-of-buffer
  "G" 'helm-end-of-buffer
  "\C-f" 'helm-next-page
  "\C-b" 'helm-previous-page)

(define-key helm-generic-files-map (kbd "M-o") 'helm-ff-run-switch-other-window)
(define-key helm-buffer-map (kbd "M-o") 'helm-buffer-switch-other-window)
(define-key helm-moccur-map (kbd "M-o") 'helm-moccur-run-goto-line-ow)
(define-key helm-grep-map (kbd "M-o") 'helm-grep-run-other-window-action)
(define-key helm-map (kbd "C-/") 'helm-quit-and-find-file)

(dolist (map (list helm-find-files-map helm-read-file-map))
  (define-keys map
    "M-o" 'helm-ff-run-switch-other-window
    "C-/" 'helm-ff-run-find-sh-command
    "M-h" 'helm-find-files-up-one-level
    "M-l" 'helm-execute-persistent-action
    "C-l" nil)) ; So the header displays the above binding.

(provide 'init-evil-helm)
