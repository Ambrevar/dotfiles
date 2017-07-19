;;; mu4e

;; TODO: Multi-accounts, see 'contexts'.
;; TODO: Overview of folders and their unread / flagged e-mails. Or simply use filters?
;; TODO: Cross-folder filters?
;; TODO: Is it possible to mbsync without attachments?
;; TODO: Fast reply: do not prompt and always reply to all but myself.
;; TODO: Test PGP.
;; TODO: Try-out BBDB and replace abook. See appendix manual.
;; TODO: How to add attachments? See appendix manual.
;; TODO: Test queued e-mail with smtp-*-queue. See long example in the manual.

(require 'mu4e-maildirs-extension)

(setq
 ;; Where to save attachments
 mu4e-attachment-dir "~/temp"

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

 ;; Unicode chars for decoration might cause issues with some fonts or in terminals.
 mu4e-use-fancy-chars t

 ;; If you're using a dark theme, and the messages are hard to read, it
 ;; can help to change the luminosity, e.g.:
 shr-color-visible-luminance-min 80

 ;; Gmail-style threading.
 mu4e-headers-include-related t ; TODO: Test it.

 ;; Because default completion can be extended (e.g. Helm, Ivy).
 mu4e-completing-read-function 'completing-read

 mu4e-headers-sort-direction 'ascending)

;;; Since we sort in ascending diretion, we default to the end of buffer.
(add-hook 'mu4e-headers-found-hook 'end-of-buffer)

;;; Bindings
;;; Is it still useful when Helm is on? What about multiple inboxes?
;; (setq mu4e-maildir-shortcuts '(("/Inbox" . ?i)))

(defun mu4e-add-fortune-signature ()
  (require 'functions) ; For `call-process-to-string'.
  (setq mu4e-compose-signature
        (format "%s\n\n%s"
                user-full-name
                (call-process-to-string "fortune" "-s"))))
(add-hook 'mu4e-compose-pre-hook 'mu4e-add-fortune-signature)

(provide 'init-mu4e)
