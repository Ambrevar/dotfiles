;; Ediff in one frame.
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Ediff split horizontally by default. ediff split does not seem to
;; work. Looks like the variable is never used in the source file.
(add-hook
 'ediff-mode-hook
 (lambda ()
   (setq ediff-merge-split-window-function 'split-window-vertically)))

(provide 'mode-ediff)
