;; Ediff in one frame.
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Ediff split horizontally by default.
;; TODO: ediff split does not seem to work.
;; (setq ediff-merge-split-window-function 'split-window-horizontally)
(setq ediff-merge-split-window-function 'split-window-vertically)

(provide 'mode-ediff)
