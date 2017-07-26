;;; mu4e

;; TODO: Notifications don't work? Change `display-time-mail-icon' color in modeline.
;; TODO: Reply to all by default.
;; TODO: Is it possible to mbsync without attachments?
;; TODO: Try-out BBDB and replace abook. See appendix manual.

(when (require 'mu4e-maildirs-extension nil t)
  (mu4e-maildirs-extension))
(when (require 'mu4e-alert nil t)
  (mu4e-alert-enable-mode-line-display))

(defun mu4e-headers-unread ()
  (interactive)
  (mu4e-headers-search "flag:unread AND NOT flag:trashed"))

(setq
 ;; Where to save attachments
 mu4e-attachment-dir "~/temp"

 ;; IMAP sync.
 mu4e-maildir "~/.cache/mail"
 mu4e-get-mail-command "mbsync -a"
 mu4e-update-interval 60
 mu4e-change-filenames-when-moving t ; Needed for mbsync.

 ;; SMTP
 message-send-mail-function 'smtpmail-send-it

 ;; For reporting bugs, "C-x m", etc.
 mail-user-agent 'mu4e-user-agent
 mu4e-compose-dont-reply-to-self t

 ;; Display
 mu4e-headers-date-format "%F %R"
 mu4e-headers-fields '((:human-date . 16)
                       (:flags . 6)
                       (:mailing-list . 10)
                       (:from . 22)
                       (:subject))
 mu4e-headers-time-format "%R"
 mu4e-view-show-addresses t
 mu4e-view-show-images t
 mu4e-view-image-max-width 800
 mu4e-hide-index-messages t

 ;; If you're using a dark theme, and the messages are hard to read, it
 ;; can help to change the luminosity, e.g.:
 shr-color-visible-luminance-min 80

 ;; Gmail-style threading.
 mu4e-headers-include-related t

 ;; Because default completion can be extended (e.g. Helm, Ivy).
 mu4e-completing-read-function 'completing-read)

;; Unicode chars for decoration might cause issues with some fonts or in terminals.
;; https://github.com/djcb/mu/issues/733
;; https://github.com/djcb/mu/issues/1062
;; (setq mu4e-use-fancy-chars t)

;;; Sorting in ascending order is impeded by `mu4e-search-results-limit': the 500 oldest e-mails will be displayed first.
;;; https://github.com/djcb/mu/issues/809
;; (setq mu4e-headers-sort-direction 'ascending)
;;; Since we sort in ascending direction, we default to the end of buffer.
;; (add-hook 'mu4e-headers-found-hook 'end-of-buffer)

(defun mu4e-add-fortune-signature ()
  (require 'functions) ; For `call-process-to-string'.
  (setq mu4e-compose-signature
        (format "%s\n\n%s"
                user-full-name
                (call-process-to-string "fortune" "-s"))))
(add-hook 'mu4e-compose-pre-hook 'mu4e-add-fortune-signature)

(load "~/personal/mail/mu4e.el" t)

(provide 'init-mu4e)
