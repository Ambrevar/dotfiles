;;; mu4e

;;; REVIEW: Reply to all by default.
;;; https://github.com/djcb/mu/issues/1135
;;; TODO: Is it possible to mbsync without attachments?
;;; REVIEW: Do not cite when replying: https://github.com/djcb/mu/issues/1110.
;;; TODO: Face of `message-cited-text' does not work.
;;; REVIEW: Handle attachments in attached e-mails.
;;; See https://github.com/djcb/mu/issues/454#issuecomment-320616279.
;;; TODO: <tab> should go to next link in text e-mails too.

;; We need 'main' to setup pinentry-emacs for GPG.
(require 'main)

(when (require 'mu4e-maildirs-extension nil t)
  (mu4e-maildirs-extension))

(defun ambrevar/mu4e-headers ()
  "Like `mu4e' but show the header view.
Default to unread messages if the header buffer does not already exist."
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
 mu4e-update-interval 90
 mu4e-change-filenames-when-moving t ; Preferred for mbsync according to the man page.

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

 ;; Gmail likes format=flowed(?)
 ;; mu4e-compose-format-flowed

 ;; Also crypt to self so that we can read sent e-mails.
 mml-secure-openpgp-encrypt-to-self t

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

(defvar ambrevar/mu4e-compose-fortune-p t "Whether or not to include a fortune in the signature.")
(defun ambrevar/mu4e-add-fortune-signature ()
  (require 'functions) ; For `call-process-to-string'.
  (setq mu4e-compose-signature
        (if (and ambrevar/mu4e-compose-fortune-p
                 (executable-find "fortune"))
            (format "%s\n\n%s"
                    user-full-name
                    (ambrevar/call-process-to-string "fortune" "-s"))
          user-full-name)))
(add-hook 'mu4e-compose-pre-hook 'ambrevar/mu4e-add-fortune-signature)

;;; Make some e-mails stand out a bit.
(set-face-foreground 'mu4e-unread-face "yellow")
(set-face-attribute 'mu4e-flagged-face nil :inherit 'font-lock-warning-face)

;;; Confirmation on every mark execution is too slow to my taste.
(defun ambrevar/mu4e-mark-execute-all-no-confirm ()
  (interactive)
  (mu4e-mark-execute-all t))
(define-key mu4e-headers-mode-map "x" 'ambrevar/mu4e-mark-execute-all-no-confirm)

(when (require 'helm-mu nil t)
  (dolist (map (list mu4e-headers-mode-map mu4e-main-mode-map mu4e-view-mode-map))
    (define-key map "s" 'helm-mu)))

(defvar ambrevar/mu4e-compose-signed-p t)
(defvar ambrevar/mu4e-compose-signed-and-crypted-p nil)
(defun ambrevar/mu4e-compose-maybe-signed-and-crypted ()
  "Maybe sign or encrypt+sign message.
Message is signed or encrypted+signed when replying to a signed or encrypted
message, respectively.

Alternatively, message is signed or encrypted+signed if
`ambrevar/mu4e-compose-signed-p' or `ambrevar/mu4e-compose-signed-and-crypted-p' is
non-nil, respectively.

This function is suitable for `mu4e-compose-mode-hook'."
  (let ((msg mu4e-compose-parent-message))
    (cond
     ((or ambrevar/mu4e-compose-signed-and-crypted-p
          (and msg (member 'encrypted (mu4e-message-field msg :flags))))
      (mml-secure-message-sign-encrypt))
     ((or ambrevar/mu4e-compose-signed-p
          (and msg (member 'signed (mu4e-message-field msg :flags))))
      (mml-secure-message-sign-pgpmime)))))
(add-hook 'mu4e-compose-mode-hook 'ambrevar/mu4e-compose-maybe-signed-and-crypted)

;;; Org capture
(when (require 'org-mu4e nil t)
  (dolist (map (list mu4e-view-mode-map mu4e-headers-mode-map))
    ;; Org mode has "C-c C-t" for 'org-todo.
    (define-key map (kbd "C-c C-t") 'org-mu4e-store-and-capture))
  (setq org-mu4e-link-query-in-headers-mode nil))

;;; Gmail trash fix.
(defvar ambrevar/mu4e-move-to-trash-patterns nil
  "List of regexps to match for moving to trash instead of deleting them.
Matches are done against the :maildir field of the e-mail at
point.  See `ambrevar/mu4e-headers-move-to-trash' and
`ambrevar/mu4e-view-move-to-trash'.")

(defun ambrevar/mu4e-headers-move-to-trash ()
  (interactive)
  (let ((msg-dir (mu4e-message-field (mu4e-message-at-point) :maildir)))
    (if (not (seq-filter (lambda (re)
                           (string-match re msg-dir))
                         ambrevar/mu4e-move-to-trash-patterns))
        (mu4e-headers-mark-for-delete)
      (mu4e-mark-set 'move (funcall mu4e-trash-folder (mu4e-message-at-point)))
      (mu4e-headers-next))))

(defun ambrevar/mu4e-view-move-to-trash ()
  (interactive)
  (mu4e~view-in-headers-context
   (ambrevar/mu4e-headers-move-to-trash)
   (mu4e~headers-move (or n 1))))

;;; Don't display trashed messages in bookmarks.  This is useful for Gmail where
;;; the "delete" flag is not used.
(defvar ambrevar/mu4e-trash-folders nil
  "List of trash folders to filter out from bookmarks.")

(load "~/personal/mail/mu4e.el" t)

;; Do this after setting `ambrevar/mu4e-trash-folders'.
(dolist (bookmark mu4e-bookmarks)
  ;; TODO: Why mu4e-bookmark-query does not work here?
  (setf (car bookmark) (concat  (mapconcat (lambda (s) (format "NOT maildir:\"%s\" and " s))
                                           ambrevar/mu4e-trash-folders "")
                                (car bookmark))))

(provide 'init-mu4e)
