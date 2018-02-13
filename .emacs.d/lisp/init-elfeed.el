;;; Elfeed

(setq elfeed-db-directory (concat ambrevar/emacs-cache-folder "elfeed"))

(defun elfeed-play-with-mpv ()
  "Play entry link with mpv."
  (interactive)
  (let ((entry (if (eq major-mode 'elfeed-show-mode) elfeed-show-entry (elfeed-search-selected :single)))
        (quality-arg "")
        (quality-val (completing-read "Max height resolution (0 for unlimited): " '("0" "480" "720") nil nil)))
    (setq quality-val (string-to-number quality-val))
    (message "Opening %s with height≤%s with mpv..." (elfeed-entry-link entry) quality-val)
    (when (< 0 quality-val)
      (setq quality-arg (format "--ytdl-format=[height<=?%s]" quality-val)))
    (start-process "elfeed-mpv" nil "mpv" quality-arg (elfeed-entry-link entry))))

(defun elfeed-open-with-eww ()
  "Open in eww with `eww-readable'."
  (interactive)
  (let ((entry (if (eq major-mode 'elfeed-show-mode) elfeed-show-entry (elfeed-search-selected :single))))
    (eww  (elfeed-entry-link entry))
    (add-hook 'eww-after-render-hook 'eww-readable nil t)))

(defvar elfeed-visit-patterns
  '(("youtu\\.?be" . elfeed-play-with-mpv)
    ("phoronix" . elfeed-open-with-eww))
  "List of (regexps . function) to match against elfeed entry link to know
whether how to visit the link.")

(defun elfeed-visit-maybe-external ()
  "Visit with external function if entry link matches `elfeed-visit-patterns',
visit otherwise."
  (interactive)
  (let ((entry (if (eq major-mode 'elfeed-show-mode) elfeed-show-entry (elfeed-search-selected :single)))
        (patterns elfeed-visit-patterns))
    (while (and patterns (not (string-match (caar patterns) (elfeed-entry-link entry))))
      (setq patterns (cdr patterns)))
    (if patterns
        (funcall (cdar patterns))
      (if (eq major-mode 'elfeed-search-mode)
          (elfeed-search-browse-url)
        (elfeed-show-visit)))))

(define-key elfeed-search-mode-map "v" #'elfeed-play-in-mpv)

(defun elfeed-kill-entry ()
  "Like `elfeed-kill-buffer' but pop elfeed search."
  (interactive)
  (elfeed-kill-buffer)
  (switch-to-buffer "*elfeed-search*"))
(define-key elfeed-show-mode-map "q" #'elfeed-kill-entry)

(defun elfeed-switch-back ()
  "Back to the last elfeed buffer, entry or search."
  (interactive)
  (let ((buffer (get-buffer "*elfeed-entry*")))
    (if buffer
        (switch-to-buffer buffer)
      (elfeed))))

(load "~/personal/news/elfeed.el" t)

(provide 'init-elfeed)
