;;; mu4e

;; TODO: Notifications don't work? Change `display-time-mail-icon' color in modeline.
;; TODO: Reply to all by default.
;; TODO: Is it possible to mbsync without attachments?
;; TODO: Use abbrevs in compose mode.
;; TODO: Do not cite when replying: https://github.com/djcb/mu/issues/1110.
;; TODO: Face of `message-cited-text' does not work.
;; REVIEW: Handle attachments in attached e-mails.
;; See https://github.com/djcb/mu/issues/454#issuecomment-320616279.

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
 mu4e-update-interval 60
 mu4e-change-filenames-when-moving t ; Needed for mbsync.

 ;; SMTP
 message-send-mail-function 'smtpmail-send-it

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

;; Press "aV" to view in browser.
(add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)

;; Unicode chars for decoration might cause issues with some fonts or in terminals.
;; https://github.com/djcb/mu/issues/733
;; https://github.com/djcb/mu/issues/1062
;; (setq mu4e-use-fancy-chars t)

;;; REVIEW: Sorting in ascending order is impeded by `mu4e-search-results-limit': the 500 oldest e-mails will be displayed first.
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

;; Make unread e-mails stand out a bit.
(set-face-foreground 'mu4e-unread-face "yellow")

(when (require 'helm-mu nil t)
  ;; TODO: Preserve the search pattern in helm-mu.
  ;; See https://github.com/emacs-helm/helm-mu/issues/42.
  (dolist (map (list mu4e-headers-mode-map mu4e-main-mode-map mu4e-view-mode-map))
    (define-key map "\M-sf" 'helm-mu)))

(load "~/personal/mail/mu4e.el" t)

(provide 'init-mu4e)
