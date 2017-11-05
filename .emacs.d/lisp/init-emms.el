;;; Emms

;;; TODO: See if mpd is faster at building the db. Not so important.
;;; TODO: Change face from purple to white?
;;; TODO: Delete entry from cache? See `emms-cache-del'.

(setq emms-directory (concat emacs-cache-folder "emms"))
(emms-all)
(emms-history-load)
(emms-default-players)
(when (require 'emms-player-mpv nil t)
  ;; Don't use default players as they have poor playback support, e.g. seeking is missing for OGG.
  ;; REVIEW: mpv should not display covers.
  ;; Reported at https://github.com/dochang/emms-player-mpv/issues/8.
  (add-to-list 'emms-player-mpv-parameters "--no-audio-display")
  (setq emms-player-mpv-input-file (expand-file-name "emms-mpv-input-file" emms-directory))
  (setq emms-player-list '(emms-player-mpv emms-player-mplayer-playlist emms-player-mplayer)))

(setq emms-source-file-default-directory "~/music/"
      emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find)

(add-to-list 'emms-info-functions 'emms-info-cueinfo)
(when (executable-find "emms-print-metadata")
  (require 'emms-info-libtag)
  (add-to-list 'emms-info-functions 'emms-info-libtag)
  (delete 'emms-info-ogginfo emms-info-functions)
  (delete 'emms-info-mp3info emms-info-functions))

;;; Cache cover thumbnails.
;;; TODO: This needs a modification to upstream `emms-browser-get-cover-from-path'. Report the problem. Advice function in the meantime?
;; ((functionp 'emms-browser-covers)
;;  (emms-browser-covers (file-name-directory path) size))
(defvar emms-cache-cover-small-size 128)
(defvar emms-cache-cover-medium-size 256)
(defvar emms-cache-cover-large-size 1024) ; Emms does not use it as of this writing
;;; TODO: Sync cover cache? What if covers are updated? Implement function to scan db and remove unlinked files.
;;; TODO: `emms-browser-expand-all' is slow because of all the covers. Turn off cover cache momentarily?
;;; TODO: Async version? Maybe not needed. Test with big music folder.

(defun emms-cache-cover-filter-default (dir)
  "Select covers containing 'front' in DIR.
If none was found, fallback on all files matching 'cover'.
If none, fallback on `emms-cache-cover-filter-all'.

See `emms-cache-cover-filter'."
  (let (covers)
    ;; TODO: Refactor this code.
    (dolist (ext emms-browser-covers-file-extensions)
      (setq covers (append (file-expand-wildcards (expand-file-name (concat "*front*." ext) dir)) covers))
      (setq covers (append (file-expand-wildcards (expand-file-name (concat "*Front*." ext) dir)) covers)))
    (unless covers
      (dolist (ext emms-browser-covers-file-extensions)
        (setq covers (append (file-expand-wildcards (expand-file-name (concat "*cover*." ext) dir)) covers))
        (setq covers (append (file-expand-wildcards (expand-file-name (concat "*Cover*." ext) dir)) covers)))
      (unless covers
        (setq covers (emms-cache-cover-filter-all dir))))
    covers))

(defun emms-cache-cover-filter-all (dir)
  "Return the list of all files with `emms-browser-covers-file-extensions' in DIR.

See `emms-cache-cover-filter'."
  (let (covers)
    (dolist (ext emms-browser-covers-file-extensions)
      (setq covers (append (file-expand-wildcards (expand-file-name (concat "*." ext) dir)) covers)))))

(defvar emms-cache-cover-filter 'emms-cache-cover-filter-default
  "This filter must hold a function that takes a directory argument and returns a list of cover file names.
The list will be processed by `emms-cache-covers'.
See also `emms-cache-cover-filter-default'.")


(defun emms-cache-covers (dir size)
  "Return cached cover SIZE for album in DIR.

SIZE must be 'small, 'medium or 'large.  It will determine the
resolution of the cached file.  See `emms-cache-cover-SIZE-size'.

If cover is not cached or if cache is out-of-date, re-cache it.
If the cover is smaller than `emms-cache-cover-SIZE-size', it
need not be cached and will be used directly.

Emms assumes that you have one album per folder. This function
will always use the same cover per folder."
  (if (eq size 'large)
      ;; 'large is unused for now. Return empty.
      nil
    (let (covers
          cover
          (cover-width 0) (cover-height 0)
          (size-value (symbol-value (intern (concat "emms-cache-cover-" (symbol-name size) "-size"))))
          cache-dest-file)
      (setq covers (funcall emms-cache-cover-filter dir))
      (if (not covers)
          nil
        ;; Find best quality cover.
        (let (res)
          (dolist (c covers)
            (setq res (image-size (create-image c) t))
            ;; image-size does not error, it returns (30. 30) instead.
            (and (> (car res) 30) (> (cdr res) 30)
                 (< cover-width (car res)) (< cover-height (cdr res))
                 (setq cover-width (car res) cover-height (cdr res) cover c))))
        (if (and (>= size-value cover-width) (>= size-value cover-height))
            ;; No need to resize and cache.
            cover
          (let ((cache-dest (concat (expand-file-name "cover-cache" emms-directory) (file-name-directory cover))))
            (mkdir cache-dest t)
            (setq cache-dest-file (concat
                                   (expand-file-name "cover_" cache-dest)
                                   (symbol-name size)
                                   "." (file-name-extension cover))))
          (and (executable-find "convert")
               (or (not (file-exists-p cache-dest-file))
                   (time-less-p (nth 5 (file-attributes cache-dest-file))
                                (nth 5 (file-attributes cover)) ))
               (let (err msg)
                 ;; An Elisp function would be faster, but Emacs does not seem be be
                 ;; able to resize image files. It can resize image displays though.
                 (setq msg (with-output-to-string
                             (with-current-buffer standard-output
                               (setq err (call-process "convert" nil '(t t) nil
                                                       "-resize" (format "%sx%s" size-value size-value)
                                                       cover
                                                       cache-dest-file)))))
                 ;; (message "EMMS convert status: %s, msg %s" err msg)
                 (when (/= err 0)
                   (warn "%s" msg)
                   (setq cache-dest-file nil))))
          cache-dest-file)))))

(fset 'emms-browser-covers 'emms-cache-covers)

;;; Resume on restart.
;;; We don't use bookmarks as that could interfere with user's ones.
(with-eval-after-load 'desktop
  (add-to-list 'desktop-globals-to-save 'emms-playing-time)
  (when (emms-playlist-current-selected-track)
    (let ((time emms-playing-time))
      (setq emms-playing-time 0) ; Don't disturb the time display.
      (and (memq 'emms-player-mpv emms-player-list)
           (executable-find "mpv")
           (push "--mute=yes" emms-player-mpv-parameters))
      (emms-start)
      (sleep-for 0 300) ; This is required for the player might not be ready yet.
      ;; TODO: This 'sleep-for' is a kludge and upstream should provide a provision for it.
      (with-demoted-errors "EMMS error: %S" (emms-player-seek-to time))
      (and (memq 'emms-player-mpv emms-player-list)
           (executable-find "mpv")
           (pop emms-player-mpv-parameters)
           (call-process-shell-command (emms-player-mpv--format-command "mute") nil nil nil))
      (emms-pause))))

;;; Browse by album-artist.
(defun emms-browser-get-track-custom (track type)
  "Return TYPE from TRACK.
This function uses 'info-albumartistsort, 'info-albumartist,
'info-artistsort, 'info-originalyear, 'info-originaldate and
'info-date symbols, if available for TRACK."
  (cond ((eq type 'info-artist)
         (or (emms-track-get track 'info-albumartistsort)
             (emms-track-get track 'info-albumartist)
             (emms-track-get track 'info-artistsort)
             (emms-track-get track 'info-artist "<unknown>")))
        ((eq type 'info-year)
         (let ((date (or (emms-track-get track 'info-originaldate)
                         (emms-track-get track 'info-originalyear)
                         (emms-track-get track 'info-date)
                         (emms-track-get track 'info-year "<unknown>"))))
           (emms-extract-year-from-date date)))
        (t (emms-track-get track type "<unknown>"))))

(defun emms-extract-year-from-date (date)
  "Try to extract year part from DATE.
Return DATE if the year cannot be extracted."
  (let ((year (nth 5 (parse-time-string date))))
    (if year (number-to-string year)
      (if (string-match "^[ \t]*[0-9]\\{4\\}" date)
          (substring date (match-beginning 0) (match-end 0))
        date))))

(setq emms-browser-get-track-field-function #'emms-browser-get-track-custom)

(when (require 'helm-emms nil t)
  (setq helm-emms-default-sources
        '(helm-source-emms-dired
          helm-source-emms-files ; Disable for a huge speed-up.
          helm-source-emms-streams)))

(require 'patch-emms)

(provide 'init-emms)
