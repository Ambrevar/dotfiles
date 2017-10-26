;;; Evil+Ztree

(evil-set-initial-state 'ztree-mode 'motion)

(evil-define-key 'motion ztree-mode-map
  (kbd "<tab>") 'ztree-jump-side
  (kbd "<return>") 'ztree-perform-action
  (kbd "SPC") 'ztree-perform-soft-action

  "x" 'ztree-toggle-expand-subtree

  ;; update
  "gr" 'ztree-refresh-buffer

  ;; quit
  "q" 'quit-window
  "ZQ" 'quit-window
  "ZZ" 'quit-window)

(evil-define-minor-mode-key 'motion 'ztreediff-mode
  "C" 'ztree-diff-copy
  "D" 'ztree-diff-delete-file
  "zH" 'ztree-diff-toggle-show-filtered-files
  "d" 'ztree-diff-simple-diff-files
  "zh" 'ztree-diff-toggle-show-equal-files
  "gf" 'ztree-diff-view-file

  ;; update
  "gr" 'ztree-diff-partial-rescan
  "gR" 'ztree-diff-full-rescan)

(provide 'init-evil-ztree)
