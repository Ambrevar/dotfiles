;;; GUD

;;; Set GUD to display many windows by default.
(setq gdb-many-windows t)

;;; Change GUD many-windows layout.
(defun gdb-setup-windows ()
  "Layout the window pattern for `gdb-many-windows'.
Do not set `gdb-show-main' to true as we handle it manually here."
  (setq gdb-source-window (selected-window))
  (gdb-display-locals-buffer)
  (delete-other-windows)
  (gdb-display-stack-buffer)
  (delete-other-windows)
  (gdb-display-breakpoints-buffer)
  (delete-other-windows)

  (switch-to-buffer
   (if gud-last-last-frame
       (gud-find-file (car gud-last-last-frame))
     (if gdb-main-file
         (gud-find-file gdb-main-file)
       ;; Put buffer list in window if we can't find a source file.
       (list-buffers-noselect))))

  (split-window-horizontally)
  (other-window 1)
  (split-window nil (/ ( * (window-height) 3) 4))
  (split-window nil (/ (window-height) 3))
  (gdb-set-window-buffer (gdb-locals-buffer-name))

  (split-window-horizontally)
  (other-window 1)
  (gdb-set-window-buffer (gdb-get-buffer-create 'gdb-inferior-io))

  (other-window 1)
  (gdb-set-window-buffer gud-comint-buffer)
  (other-window 1)
  (gdb-set-window-buffer (gdb-stack-buffer-name))
  (split-window-horizontally)
  (other-window 1)
  (gdb-set-window-buffer (gdb-breakpoints-buffer-name))
  (other-window 1))

(provide 'init-gud)
