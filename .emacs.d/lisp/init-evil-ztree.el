;;; Evil+Ztree

(evil-set-initial-state 'ztree-mode 'motion)

(evil-define-key 'motion ztree-mode-map
  (kbd "<tab>") 'ztree-jump-side
  (kbd "<return>") 'ztree-perform-action
  (kbd "SPC") 'ztree-perform-soft-action
  "gr" 'ztree-refresh-buffer
  "q" 'quit-window
  "x" 'ztree-toggle-expand-subtree)

(evil-define-minor-mode-key 'motion 'ztreediff-mode
  "C" 'ztree-diff-copy
  "D" 'ztree-diff-delete-file
  "zH" 'ztree-diff-toggle-show-filtered-files
  "R" 'ztree-diff-full-rescan
  "d" 'ztree-diff-simple-diff-files
  "zh" 'ztree-diff-toggle-show-equal-files
  "r" 'ztree-diff-partial-rescan
  "gf" 'ztree-diff-view-file)

(provide 'init-evil-ztree)
