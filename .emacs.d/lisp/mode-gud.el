;; GUD

;; Set GUD to display many windows by default.
;; (setq gdb-show-main t)
(setq gdb-many-windows t)

;; Change GUD many-windows layout.
(defun gdb-setup-windows ()
  "Layout the window pattern for `gdb-many-windows'."
  (setq gdb-source-window (selected-window))
  (gdb-display-locals-buffer)
  (delete-other-windows)
  (gdb-display-stack-buffer)
  (delete-other-windows)
  (gdb-display-breakpoints-buffer)
  (delete-other-windows)

  ;; TODO: this does not behave the same on Emacs 23 and 24.
  ;; This may be needed for gud/pdb.
  ;; (defadvice pop-to-buffer (before cancel-other-window first)
  ;;   (ad-set-arg 1 nil))
  ;; (ad-activate 'pop-to-buffer)
  (switch-to-buffer
   (if gud-last-last-frame
       (gud-find-file (car gud-last-last-frame))
     (if gdb-main-file
         (gud-find-file gdb-main-file)
       ;; Put buffer list in window if we can't find a source file.
       (list-buffers-noselect))))

  (split-window-horizontally)
  (other-window 1)
  (split-window nil ( / ( * (window-height) 3) 4))
  (split-window nil ( / (window-height) 3))
  (gdb-set-window-buffer (gdb-locals-buffer-name))
  (other-window 1)
  (gdb-set-window-buffer gud-comint-buffer)
  (when (and
         (boundp 'gdb-use-separate-io-buffer)
         gdb-use-separate-io-buffer)
    (split-window-horizontally)
    (other-window 1)
    (gdb-set-window-buffer
     (gdb-get-buffer-create 'gdb-inferior-io)))
  (other-window 1)
  (gdb-set-window-buffer (gdb-stack-buffer-name))
  (split-window-horizontally)
  (other-window 1)
  (gdb-set-window-buffer (gdb-breakpoints-buffer-name))
  (other-window 1))

(provide 'mode-gud)
