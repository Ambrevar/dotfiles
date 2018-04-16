;; TODO: Publish helm-eww package.

(require 'helm)

(defvar helm-eww-buffer-max-length 51
  "Max length of EWW buffer names before truncating.
When disabled (nil) use the longest buffer-name length found.

See `helm-buffer-max-length`.  This variable's default is so that
the EWW title starts at the column of the open parenthesis in
`helm-buffers-list' detailed view.")

(defun helm-eww-toggle-buffers-details ()
  (interactive)
  (with-helm-alive-p
    (let* ((buf (helm-get-selection))
           ;; `helm-buffer--get-preselection' uses `helm-buffer-max-length'.
           (helm-buffer-max-length helm-eww-buffer-max-length)
           (preselect (helm-buffer--get-preselection buf)))
      (setq helm-buffer-details-flag (not helm-buffer-details-flag))
      ;; TODO: `helm-force-update' seems to be necessary to be necessary to
      ;; update the buffer live.  It is not the case for helm-buffers-list
      ;; though.  Why?
      (helm-force-update (lambda ()
                           (helm-awhile (re-search-forward preselect nil t)
                             (helm-mark-current-line)
                             (when (equal buf (helm-get-selection))
                               (cl-return t))))))))
(put 'helm-eww-toggle-buffers-details 'helm-only t)

(defun helm-eww-new-buffer (url)
  "Fetch URL and render the page in a new buffer.
If the input doesn't look like an URL or a domain name, the
word(s) will be searched for via `eww-search-prefix'."
  (let ((b (generate-new-buffer "*eww*")))
    (with-current-buffer b
      (eww-mode)
      (eww url))
    b))

(defun helm-eww-switch-buffers (_candidate)
  "Open marked URL(s) in EWW.
If more than one URL is marked, or with prefix argument, open in
new buffer."
  (let ((c (helm-marked-candidates)))
    (if (and (= 1 (length c))
             (null helm-current-prefix-arg))
        (eww (car c))
      (helm-window-show-buffers (mapcar 'helm-eww-new-buffer c)))))
(put 'helm-eww-switch-buffers 'helm-only t)

(defun helm-eww-switch-other-window (_candidate)
  "Open marked URL(s) in other windows."
  (helm-window-show-buffers (mapcar 'helm-eww-new-buffer (helm-marked-candidates)) t))
(put 'helm-eww-switch-other-window 'helm-only t)

(defun helm-eww-switch-other-frame (candidate)
  "Open URL of marked CANDIDATE in other frame."
  (switch-to-buffer-other-frame (helm-eww-new-buffer candidate)))
(put 'helm-eww-switch-other-frame 'helm-only t)

(defun helm-eww-browser-with-external-browser (_candidates)
  "Like `eww-browse-with-external-browser' with multiple URLs."
  (dolist (c (helm-marked-candidates))
    (eww-browse-with-external-browser
     (if (bufferp c)
         (with-current-buffer c
           (plist-get eww-data :url))
       c))))
(put 'helm-eww-browser-with-external-browser 'helm-only t)


(defvar helm-eww-buffers-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c o") 'helm-buffer-switch-other-window)
    (define-key map (kbd "C-c C-o") 'helm-buffer-switch-other-frame)
    (define-key map (kbd "M-D") 'helm-buffer-run-kill-buffers)
    (define-key map (kbd "C-]") 'helm-eww-toggle-buffers-details)
    map)
  "Keymap for browser source in Helm.")

;; Inspired by `helm-highlight-buffers'.
(defun helm-eww-highlight-buffers (buffers)
  "Transformer function to highlight BUFFERS list.
Should be called after others transformers i.e (boring buffers)."
  (cl-loop for i in buffers
           for (url title) = (with-current-buffer i (list (eww-current-url) (plist-get eww-data :title)))
           for truncbuf = (if (> (string-width url) helm-eww-buffer-max-length)
                              (helm-substring-by-width
                               url helm-eww-buffer-max-length
                               helm-buffers-end-truncated-string)
                            (concat url
                                    (make-string
                                     (- (+ helm-eww-buffer-max-length
                                           (length helm-buffers-end-truncated-string))
                                        (string-width url))
                                     ? )))
           collect (let ((helm-pattern (helm-buffers--pattern-sans-filters
                                        (and helm-buffers-fuzzy-matching ""))))
                     (cons (if helm-buffer-details-flag
                               (concat
                                (funcall helm-fuzzy-matching-highlight-fn truncbuf)
                                "  " (propertize title 'face 'helm-buffer-process))
                             (funcall helm-fuzzy-matching-highlight-fn url))
                           (get-buffer i)))))

(defun helm-eww-buffers-build-source ()
  "Build source for EWW buffers.
See `helm-eww' for more details."
  (helm-build-sync-source "EWW buffers"
    :candidates (seq-filter (lambda (b) (with-current-buffer b (derived-mode-p 'eww-mode))) (buffer-list))
    :candidate-transformer 'helm-eww-highlight-buffers
    :action '(("Switch to buffer(s)" . helm-buffer-switch-buffers)
              ("Open URL(s) in external browser" . helm-eww-browser-with-external-browser)
              ("Switch to buffer(s) in other window `C-c o'" . helm-buffer-switch-buffers-other-window)
              ("Switch to buffer in other frame `C-c C-o'" . switch-to-buffer-other-frame)
              ("Kill buffer(s) `M-D`" . helm-kill-marked-buffers))
    ;; When follow-mode is on, the persistent-action allows for multiple candidate selection.
    :persistent-action 'helm-buffers-list-persistent-action
    :keymap helm-eww-buffers-map))
(add-to-list 'helm-source-names-using-follow "EWW buffers")

(defun helm-eww-buffers ()
  "Preconfigured `helm' to list EWW buffers."
  (interactive)
  (helm :sources (helm-eww-buffers-build-source)
        :truncate-lines t
        :buffer "*helm-eww-buffers*"))


(defvar helm-eww-bookmarks-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c o") 'helm-eww-switch-other-window)
    (define-key map (kbd "C-c C-o") 'helm-eww-switch-other-frame)
    (define-key map (kbd "M-D") 'helm-eww-bookmarks-delete)
    (define-key map (kbd "C-]") 'helm-eww-toggle-buffers-details)
    map)
  "Keymap for bookmarks source in Helm.")

(defun helm-eww-highlight-bookmarks (candidates)
  "Transformer function to highlight CANDIDATES list.
CANDIDATES are (URL . TITLE)"
  (cl-loop for (url title) in candidates
           for truncbuf = (if (> (string-width url) helm-eww-buffer-max-length)
                              (helm-substring-by-width
                               url helm-eww-buffer-max-length
                               helm-buffers-end-truncated-string)
                            (concat url
                                    (make-string
                                     (- (+ helm-eww-buffer-max-length
                                           (length helm-buffers-end-truncated-string))
                                        (string-width url))
                                     ? )))
           collect (let ((helm-pattern (helm-buffers--pattern-sans-filters
                                        (and helm-buffers-fuzzy-matching ""))))
                     (cons (if helm-buffer-details-flag
                               (concat
                                (funcall helm-fuzzy-matching-highlight-fn truncbuf)
                                "  " (propertize title 'face 'helm-buffer-process))
                             (funcall helm-fuzzy-matching-highlight-fn url))
                           url))))

(defun helm-eww-bookmarks-delete (_candidate)
  "Delete all bookmarks with the URLs of the candidates."
  (dolist (c (helm-marked-candidates))
    (setq eww-bookmarks (seq-remove (lambda (b) (string= (plist-get b :url) c))
                                    eww-bookmarks)))
  (eww-write-bookmarks))
(put 'helm-eww-bookmarks-kill 'helm-only t)

(defun helm-eww-bookmarks-build-source ()
  "Build source for EWW bookmarks.
See `helm-eww-bookmarks' for more details."
  ;; Bookmarks are only first loaded when `eww-bookmark-prepare' is called.
  ;; This can be too late for us, so we do it here.
  (unless eww-bookmarks
    (eww-read-bookmarks))
  (helm-build-sync-source "EWW bookmarks"
    :candidates (mapcar (lambda (e) (list (plist-get e :url) (plist-get e :title))) eww-bookmarks)
    :candidate-transformer 'helm-eww-highlight-bookmarks
    :candidate-number-limit 1000
    :action '(("Open URL(s)" . helm-eww-switch-buffers)
              ("Open URL(s) in external browser" . helm-eww-browser-with-external-browser)
              ("Open URL(s) in other window `C-c o'" . helm-eww-switch-other-window)
              ("Open URL in other frame `C-c C-o'" . helm-eww-switch-other-frame)
              ("Delete bookmark(s) `M-D`" . helm-eww-bookmarks-delete))
    :keymap helm-eww-bookmarks-map))

(defun helm-eww-bookmarks ()
  "Preconfigured `helm' to list EWW bookmarks."
  (interactive)
  (helm :sources (helm-eww-bookmarks-build-source)
        :truncate-lines t
        :buffer "*helm-eww-bookmarks*"))


(defvar helm-eww-history-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-]") 'helm-eww-toggle-buffers-details)
    map)
  "Keymap for EWW history source in Helm.")

(defun helm-eww-highlight-history (candidates)
  "Transformer function to highlight CANDIDATES list.
CANDIDATES are `eww-history' elements."
  (cl-loop for e in candidates
           for (url title) = (list (plist-get e :url) (plist-get e :title))
           for truncbuf = (if (> (string-width url) helm-eww-buffer-max-length)
                              (helm-substring-by-width
                               url helm-eww-buffer-max-length
                               helm-buffers-end-truncated-string)
                            (concat url
                                    (make-string
                                     (- (+ helm-eww-buffer-max-length
                                           (length helm-buffers-end-truncated-string))
                                        (string-width url))
                                     ? )))
           collect (let ((helm-pattern (helm-buffers--pattern-sans-filters
                                        (and helm-buffers-fuzzy-matching ""))))
                     (cons (if helm-buffer-details-flag
                               (concat
                                (funcall helm-fuzzy-matching-highlight-fn truncbuf)
                                "  " (propertize title 'face 'helm-buffer-process))
                             (funcall helm-fuzzy-matching-highlight-fn url))
                           e))))

(defun helm-eww-history-build-source ()
  "Build source for EWW history.
See `helm-eww-bookmarks' for more details."
  (helm-build-sync-source "EWW history"
    :candidates eww-history
    :candidate-transformer 'helm-eww-highlight-history
    :candidate-number-limit 1000
    :nomark t      ; TODO: Enable marking and use current candidate only for `eww-restore-history'.
    :action '(("Go back to" . eww-restore-history)
              ;; ("Open URI (s) in other window `C-c o'" . helm-buffer-switch-buffers-other-window)
              ;; ("Switch to buffer in other frame `C-c C-o'" . switch-to-buffer-other-frame)
              ;; ("Kill buffer(s) `M-D`" . helm-kill-marked-buffers) ; TODO: Kill action?
              )
    ;; TODO: Persistent action that does not change history.  Need to fix `eww-forward-url' first.
    :keymap helm-eww-history-map))

(defun helm-eww-history ()
  "Preconfigured `helm' to list EWW history."
  (interactive)
  (unless (derived-mode-p 'eww-mode)
    (user-error "Not a EWW buffer"))
  (helm :sources (list (helm-eww-history-build-source))
        :truncate-lines t
        :buffer "*helm-eww-history*"))


(defvar helm-eww-new
  (helm-build-dummy-source "Open new page"
    :action (helm-make-actions "Open new page" 'helm-eww-new-buffer)))

;; All in one.
(defun helm-eww ()
  "Preconfigured `helm' to list all EWW sources."
  (interactive)
  (helm :sources (list 'helm-eww-new
                       (helm-eww-buffers-build-source)
                       ;; TODO: Can we add history as well?  It would make sense if we only allow history entries in new buffers.
                       ;; (helm-eww-history-build-source)
                       (helm-eww-bookmarks-build-source))
        :truncate-lines t
        :buffer "*helm-eww*"))

(provide 'package-helm-eww)
