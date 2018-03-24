;;; EWW

(setq eww-bookmarks-directory "~/personal/bookmarks"
      eww-download-directory "~/temp")

(defun ambrevar/eww-next-url (&optional backward)
  "Like `eww-next-url' but if no next URL is found, go to next URL numerically.
The URL index is the last number after the last '/'."
  (interactive)
  (condition-case nil
      (if backward
          (eww-previous-url)
        (eww-next-url))
    (user-error
     (when (eq major-mode 'eww-mode)
       (require 'rx)
       (let* ((url (plist-get eww-data :url))
              (re (rx (group (one-or-more digit))
                      (zero-or-more (not (any "/")))
                      line-end)))
         (if (and (string-match re url)
                  (or (not backward)
                      (> (string-to-number (match-string 1 url)) 0)))
             (eww
              (replace-regexp-in-string
               re
               (format (format "%%0.%dd" (length (match-string 1 url))) ; In case matched number is zero-padded.
                       (funcall (if backward '1- '1+) (string-to-number (match-string 1 url))))
               url nil nil 1))
           (message "No index in URL.")))))))

(defun ambrevar/eww-previous-url ()
  "Like `eww-previous-url' but if no next URL is found, go to next URL numerically.
The URL index is the last number after the last '/'."
  (interactive)
  (ambrevar/eww-next-url 'backward))

(defun ambrevar/eww-switch-back ()
  "Switch to the *eww* buffer."
  (interactive)
  (let ((buffer (get-buffer "*eww*")))
    (if buffer
        (switch-to-buffer buffer)
      (call-interactively 'eww))))

(provide 'init-eww)
