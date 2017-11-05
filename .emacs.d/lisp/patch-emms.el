;;; REVIEW: Browser does not display year when it's only stored in 'info-date.
;;; See the mailing list.
(defun emms-browser-format-line (bdata &optional target)
  "Return a propertized string to be inserted in the buffer."
  (unless target
    (setq target 'browser))
  (let* ((name (or (emms-browser-bdata-name bdata) "misc"))
         (level (emms-browser-bdata-level bdata))
         (type (emms-browser-bdata-type bdata))
         (indent (emms-browser-make-indent level))
         (track (emms-browser-bdata-first-track bdata))
         (path (emms-track-get track 'name))
         (face (emms-browser-get-face bdata))
         (format (emms-browser-get-format bdata target))
         (props (list 'emms-browser-bdata bdata))
         (format-choices
          `(("i" . ,indent)
            ("n" . ,name)
            ;; PATCH
            ;; ("y" . ,(emms-track-get track 'info-year))
            ("y" . ,(emms-track-get track 'info-date))
            ("A" . ,(emms-track-get track 'info-album))
            ("a" . ,(emms-track-get track 'info-artist))
            ("C" . ,(emms-track-get track 'info-composer))
            ("p" . ,(emms-track-get track 'info-performer))
            ("t" . ,(emms-track-get track 'info-title))
            ("D" . ,(emms-browser-disc-number track))
            ("T" . ,(emms-browser-track-number track))
            ("d" . ,(emms-browser-track-duration track))))
         str)
    (when (equal type 'info-album)
      (setq format-choices (append format-choices
                                   `(("cS" . ,(emms-browser-get-cover-str path 'small))
                                     ("cM" . ,(emms-browser-get-cover-str path 'medium))
                                     ("cL" . ,(emms-browser-get-cover-str path 'large))))))


    (when (functionp format)
      ;; (message "EMMS bdata %s" bdata)
      ;; (message "EMMS format-choices %s" format-choices)
      (setq format (funcall format bdata format-choices)))

    (setq str
          (with-temp-buffer
            (insert format)
            (goto-char (point-min))
            (let ((start (point-min)))
              ;; jump over any image
              (when (re-search-forward "%c[SML]" nil t)
                (setq start (point)))
              ;; jump over the indent
              (when (re-search-forward "%i" nil t)
                (setq start (point)))
              (add-text-properties start (point-max)
                                   (list 'face face)))
            (buffer-string)))

    (setq str (emms-browser-format-spec str format-choices))

    ;; give tracks a 'boost' if they're not top-level
    ;; (covers take up an extra space)
    (when (and (eq type 'info-title)
               (not (string= indent "")))
      (setq str (concat " " str)))

    ;; if we're in playlist mode, add a track
    (when (and (eq target 'playlist)
               (eq type 'info-title))
      (setq props
            (append props `(emms-track ,track))))

    ;; add properties to the whole string
    (add-text-properties 0 (length str) props str)
    str))


;;; REVIEW: Sort albums by year in browser.
;;; See the mailing list.
(defun emms-browser-year-number (track)
  "Return a string representation of a track's year.
This will be in the form '(1998) '."
  (let ((year (or (emms-track-get track 'info-year) (emms-track-get track 'info-date))))
    (if (or (not (stringp year)) (string= year "0"))
        ""
      (concat
       "(" year ") "))))

(provide 'patch-emms)
