;;; mu4e

;;; REVIEW: Reply to all by default.
;;; https://github.com/djcb/mu/issues/1135
;;; TODO: Is it possible to mbsync without attachments?
;;; REVIEW: Use abbrevs in compose mode.
;;; Fixed upstream.  See https://github.com/djcb/mu/issues/1119.
;;; REVIEW: Do not cite when replying: https://github.com/djcb/mu/issues/1110.
;;; TODO: Face of `message-cited-text' does not work.
;;; REVIEW: Handle attachments in attached e-mails.
;;; See https://github.com/djcb/mu/issues/454#issuecomment-320616279.
;;; TODO: <tab> should go to next link in text e-mails too.

;; Emacs pinentry for GPG.
(require 'main)

(when (require 'mu4e-maildirs-extension nil t)
  (mu4e-maildirs-extension))

(defun mu4e-headers ()
  "Like `mu4e' but show the header view.
Default to unread messages if no"
  (interactive)
  (mu4e~start)
  (if (get-buffer "*mu4e-headers*" )
      (switch-to-buffer "*mu4e-headers*")
    (mu4e-headers-search "flag:unread AND NOT flag:trashed")))

(setq
 ;; Attachments
 mu4e-attachment-dir "~/temp"
 mu4e-save-multiple-attachments-without-asking t

 ;; IMAP sync.
 mu4e-maildir "~/.cache/mail"
 mu4e-get-mail-command "mbsync -a"
 mu4e-update-interval 120
 mu4e-change-filenames-when-moving t ; Needed for mbsync.

 ;; SMTP
 message-send-mail-function 'smtpmail-send-it

 ;; Don't bother me with context on startup.
 mu4e-context-policy nil

 ;; Don't keep sent e-mail buffer.
 message-kill-buffer-on-exit t

 ;; For reporting bugs, "C-x m", etc.
 mail-user-agent 'mu4e-user-agent
 mu4e-compose-dont-reply-to-self t

 ;; Display
 mu4e-headers-date-format "%F %R"
 mu4e-headers-fields '((:human-date . 16)
                       (:flags . 6)
                       (:size . 6)
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

;;; Press "aV" to view in browser.
(add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)

;;; Unicode chars for decoration might cause issues with some fonts or in terminals.
;;; https://github.com/djcb/mu/issues/733
;;; https://github.com/djcb/mu/issues/1062
;; (setq mu4e-use-fancy-chars t)

;;; REVIEW: Sorting in ascending order is impeded by `mu4e-search-results-limit': the 500 oldest e-mails will be displayed first.
;;; https://github.com/djcb/mu/issues/809
;; (setq mu4e-headers-sort-direction 'ascending)
;;; Since we sort in ascending direction, we default to the end of buffer.
;; (add-hook 'mu4e-headers-found-hook 'end-of-buffer)

(defvar mu4e-compose-fortune-p t "Whether or not to include a fortune in the signature.")
(defun mu4e-add-fortune-signature ()
  (require 'functions) ; For `call-process-to-string'.
  (setq mu4e-compose-signature
        (if mu4e-compose-fortune-p
            (format "%s\n\n%s"
                    user-full-name
                    (call-process-to-string "fortune" "-s"))
          user-full-name)))
(add-hook 'mu4e-compose-pre-hook 'mu4e-add-fortune-signature)

;;; Make some e-mails stand out a bit.
(set-face-foreground 'mu4e-unread-face "yellow")
(set-face-attribute 'mu4e-flagged-face nil :inherit 'font-lock-warning-face)

;;; Confirmation on every mark execution is too slow to my taste.
(defun mu4e-mark-execute-all-no-confirm ()
  (interactive)
  (mu4e-mark-execute-all t))
(define-key mu4e-headers-mode-map "x" 'mu4e-mark-execute-all-no-confirm)

(when (require 'helm-mu nil t)
  (dolist (map (list mu4e-headers-mode-map mu4e-main-mode-map mu4e-view-mode-map))
    (define-key map "s" 'helm-mu)))

(load "~/personal/mail/mu4e.el" t)

(provide 'init-mu4e)
