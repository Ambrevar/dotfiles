;;; EWW

(setq eww-bookmarks-directory "~/personal/bookmarks"
      eww-download-directory "~/temp")

(defun eww-switch-back ()
  "Switch to the *eww* buffer."
  (interactive)
  (let ((buffer (get-buffer "*eww*")))
    (if buffer
        (switch-to-buffer buffer)
      (eww))))

(provide 'init-eww)
