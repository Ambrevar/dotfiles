;;; EWW

;; DONE: Bookmarks: Check if count is correct after saving.
;; DONE: Bookmarks: Check sorting.
;; DONE: Bookmarks: Fix encoding.

;; TODO: Fix `eww-forward-url' infinite forwarding.

;; TODO: Extend the history / bookmarks view to display tags, etc.

;; TODO: Make something useful with the tags.  Helm function?  Could chain two
;; functions: tag selection then filtered bookmark selection, then tag selection
;; again, etc.
;; Alternative: use special syntax in prompt, like find-files does.  This does
;; not allow for listing tags though.

(require 'package-helm-eww nil t)

(setq eww-bookmarks-directory "~/personal/bookmarks"
      eww-download-directory "~/temp")
;; (add-to-list 'auto-mode-alist '("eww-bookmarks$" . emacs-lisp-mode))

(defun ambrevar/eww-copy-page-title ()
  "Copy the URL of the current page into the kill ring."
  (interactive)
  (message "%s" (plist-get eww-data :title))
  (kill-new (plist-get eww-data :title)))

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

(defun ambrevar/eww-reload-all (&optional buffers)
  "Like `eww-reload' but for multiple EWW BUFFERS.
If BUFFERS is not specified, then reload all buffers."
  (interactive)
  (dolist (b (or buffers (buffer-list)))
    (with-current-buffer b
      (when (derived-mode-p 'eww-mode)
        (eww-reload)))))

(defun ambrevar/eww-switch-back ()
  "Switch to the *eww* buffer."
  (interactive)
  (let (buffer-info)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (derived-mode-p 'eww-mode)
          (push buffer buffer-info))))
    (setq buffer-info (nreverse buffer-info))
    (if buffer-info
        (if (derived-mode-p 'eww-mode)
            (if (fboundp 'helm-eww)
                (helm-eww)
              (switch-to-buffer (completing-read "EWW: " (mapcar 'buffer-name buffer-info))))
          (switch-to-buffer (car buffer-info)))
      (call-interactively 'eww))))

(defun ambrevar/eww (url)
  "Fetch URL and render the page.
If the input doesn't look like an URL or a domain name, the
word(s) will be searched for via `eww-search-prefix'."
  (interactive
   (let* ((uris (eww-suggested-uris))
          (prompt (concat "Enter URL or keywords: "))) ; PATCH
     (list (read-string prompt (car uris) nil uris)))) ; PATCH
  (setq url (eww--dwim-expand-url url))
  (pop-to-buffer-same-window
   (if (eq major-mode 'eww-mode)
       (current-buffer)
     (get-buffer-create "*eww*")))
  (eww-setup-buffer)
  ;; Check whether the domain only uses "Highly Restricted" Unicode
  ;; IDNA characters.  If not, transform to punycode to indicate that
  ;; there may be funny business going on.
  (let ((parsed (url-generic-parse-url url)))
    (when (url-host parsed)
      (unless (puny-highly-restrictive-domain-p (url-host parsed))
        (setf (url-host parsed) (puny-encode-domain (url-host parsed)))
        (setq url (url-recreate-url parsed)))))
  (plist-put eww-data :url url)
  (plist-put eww-data :title "")
  (eww-update-header-line-format)
  (let ((inhibit-read-only t))
    (insert (format "Loading %s..." url))
    (goto-char (point-min)))
  (url-retrieve url 'eww-render
                (list url nil (current-buffer))))
(advice-add 'eww :override 'ambrevar/eww)

(defun ambrevar/eww-open-in-new-buffer (url)
  "Fetch URL and render the page.
If the input doesn't look like an URL or a domain name, the
word(s) will be searched for via `eww-search-prefix'."
  (interactive
   (let* ((uris (eww-suggested-uris))
          (prompt (concat "Open URL or keywords in new buffer: ")))
     (list (read-string prompt (car uris) nil uris))))
  (setq url (eww--dwim-expand-url url))
  (pop-to-buffer-same-window
   (if (eq major-mode 'eww-mode)
       (clone-buffer)
     (generate-new-buffer "*eww*")))
  (unless (equal url (eww-current-url))
    (eww-mode)
    (eww (if (consp url) (car url) url))))
(advice-add 'eww-open-in-new-buffer :override 'ambrevar/eww-open-in-new-buffer)

(defun ambrevar/eww-name-buffer-with-title ()
  "Include the page title in current EWW buffer name."
  (interactive)
  (when (derived-mode-p 'eww-mode)
    (rename-buffer (format "*eww: %s*" (plist-get eww-data :title)) t)))

(defun ambrevar/eww-update-header-line-format ()
  (setq header-line-format
        (and eww-header-line-format
             (let ((title (plist-get eww-data :title))
                   (peer (plist-get eww-data :peer)))
               (when (zerop (length title))
                 (setq title "[untitled]"))
               ;; This connection has is https.
               (when peer
                 (setq title
                       (propertize title 'face
                                   (if (plist-get peer :warnings)
                                       'eww-invalid-certificate
                                     'eww-valid-certificate))))
               (replace-regexp-in-string
                "%" "%%"
                (format-spec
                 eww-header-line-format
                 `((?u . ,(or (plist-get eww-data :url) ""))
                   (?t . ,title)))))))
  ;; PATCH
  (ambrevar/eww-name-buffer-with-title))
(advice-add 'eww-update-header-line-format :override 'ambrevar/eww-update-header-line-format)

;; TODO: Implement quickmarks.  Then merge them.
;; TODO: Implement search engines.  Then merge them too.
;; If only one word is found as a non-ambiguous mark, use it as quickmark.
;; If several words and first word is a non-ambiguous mark where search-engine
;; is provided, search.
;; Search-engine is either appended if not starting with https?, or a full
;; URL otherwise.  Append search terms like eww already does.  Use %s as a place
;; holder.
;; TODO: Add bookmark editing functions such as edit title, tags, quickmark,
;; search-engine.  Use eww-buffers and Helm.
(defun ambrevar/eww-add-bookmark ()
  "Bookmark the current page."
  (interactive)
  (eww-read-bookmarks)
  (let (tag-list)
    (dolist (bookmark eww-bookmarks)
      (when (equal
             ;; PATCH: Ignore protocol when sorting.
             ;; TODO: Include "sort-bookmarks": Warn for unique tags, warn for same URL up to paragraph.  Make this customizable.
             (replace-regexp-in-string "^https?" "" (plist-get eww-data :url))
             (replace-regexp-in-string "^https?" "" (plist-get bookmark :url)))
        (user-error "Already bookmarked"))
      (setq tag-list (append tag-list (plist-get bookmark :tags))))
    (delete-duplicates tag-list)
    (let ((tags (completing-read-multiple "Tags for bookmark (comma separated): " tag-list))
          (title (replace-regexp-in-string "[\n\t\r]" " "
                                           (plist-get eww-data :title))))
      (setq title (replace-regexp-in-string "\\` +\\| +\\'" "" title))
      (push `(:url ,(plist-get eww-data :url)
                   :title ,title
                   :time ,(current-time-string)
                   ,@(if tags (list :tags tags)))
            eww-bookmarks)
      (eww-write-bookmarks)
      (message "Bookmarked %s (%s)" (plist-get eww-data :url)
               (plist-get eww-data :title)))))
(advice-add 'eww-add-bookmark :override 'ambrevar/eww-add-bookmark)

(defun ambrevar/eww-write-bookmarks ()
  (require 'rx)
  ;; PATCH
  (setq eww-bookmarks
        (sort eww-bookmarks
              (lambda (a b) (string<
                             ;; Ignore protocol when sorting.
                             (replace-regexp-in-string "^[a-zA-Z]+://" "" (plist-get a :url))
                             (replace-regexp-in-string "^[a-zA-Z]+://" "" (plist-get b :url))))))
  (with-temp-file (expand-file-name "eww-bookmarks" eww-bookmarks-directory)
    ;; PATCH: Remove newline for tags.
    (insert
     (replace-regexp-in-string
      (rx ":tags" ?\n (1+ space)) ":tags "
      (with-temp-buffer
        (insert ";; Auto-generated file; don't edit -*- mode: emacs-lisp; -*-\n")
        (pp eww-bookmarks (current-buffer))
        (buffer-string))))))
(advice-add 'eww-write-bookmarks :override 'ambrevar/eww-write-bookmarks)

(provide 'init-eww)
