;; Ediff in one frame.
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Ediff split horizontally by default.
;; TODO: ediff split does not seem to work.
(lambda ()
  (setq ediff-merge-split-window-function 'split-window-horizontally))

(provide 'mode-ediff)
