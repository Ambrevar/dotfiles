;;; Emms

;;; TODO: See if mpd is faster at building the db. Not so important.
;;; TODO: Change face from purple to white?
;;; TODO: Delete entry from cache? See `emms-cache-del'.

(emms-all)
;; TODO: emms-all causes some "require"d files to be loaded twice if called after, say, emms-browser was loaded.
(emms-history-load)
(emms-default-players)
(when (require 'emms-player-mpv nil t)
  ;; Don't use default players as they have poor playback support, e.g. seeking is missing for OGG.
  ;; REVIEW: mpv should not display covers.
  ;; Reported at https://github.com/dochang/emms-player-mpv/issues/8.
  (add-to-list 'emms-player-mpv-parameters "--no-audio-display")
  (setq emms-player-mpv-input-file (expand-file-name "emms-mpv-input-file" emms-directory))
  (setq emms-player-list '(emms-player-mpv emms-player-mplayer-playlist emms-player-mplayer)))

(setq emms-source-file-default-directory (expand-file-name "~/music")
      emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find)

(add-to-list 'emms-info-functions 'emms-info-cueinfo)
(if (executable-find "emms-print-metadata")
    (progn
      (require 'emms-info-libtag)
      (add-to-list 'emms-info-functions 'emms-info-libtag)
      (delete 'emms-info-ogginfo emms-info-functions)
      (delete 'emms-info-mp3info emms-info-functions))
  (add-to-list 'emms-info-functions 'emms-info-ogginfo)
  (add-to-list 'emms-info-functions 'emms-info-mp3info))

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
(defun ambrevar/emms-browser-get-track-custom (track type)
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
           (emms-format-date-to-year date)))
        (t (emms-track-get track type "<unknown>"))))

(setq emms-browser-get-track-field-function #'ambrevar/emms-browser-get-track-custom)

(when (require 'helm-emms nil t)
  (setq helm-emms-default-sources
        '(helm-source-emms-dired
          helm-source-emms-files ; Disable for a huge speed-up.
          helm-source-emms-streams)))

;;; Cover thumbnails.
(setq emms-browser-covers 'emms-browser-cache-thumbnail)

(defun ambrevar/emms-browser-add-tracks-and-maybe-play ()
  "Like `emms-browser-add-tracks' but play immediately if nothing
is currently playing."
  (interactive)
  (if emms-player-paused-p
      (emms-browser-add-tracks-and-play)
    (emms-browser-add-tracks)))
(define-key emms-browser-mode-map (kbd "<return>") 'ambrevar/emms-browser-add-tracks-and-maybe-play)

;;; Display album in playlist
(defun ambrevar/emms-artist-album-track-and-title-format (bdata fmt)
  (concat
   "%i"
   (let ((artist (emms-browser-format-elem fmt "a")))
     (if (not artist)
         "%n"                           ; If unknown, display the filename.
       (concat
        "%a - "
        (let ((album (emms-browser-format-elem fmt "A")))
          (if album "%A - " ""))
        (let ((disc (emms-browser-format-elem fmt "D")))
          (if (and disc (not (string= disc ""))) "%D/" ""))
        (let ((track (emms-browser-format-elem fmt "T")))
          (if (and track (not (string= track "0")))
              "%T. "
            ""))
        "%t [%d]")))))
(setq emms-browser-playlist-info-title-format 'ambrevar/emms-artist-album-track-and-title-format)
;; Display disc number in browser
(defun ambrevar/emms-browser-track-artist-and-title-format (bdata fmt)
  (concat
   "%i"
   (let ((disc (emms-browser-format-elem fmt "D")))
     (if (and disc (not (string= disc "")))
         "%D/"))
   (let ((track (emms-browser-format-elem fmt "T")))
     (if (and track (not (string= track "0")))
         "%T. "
       ""))
   "%n"))
(setq emms-browser-info-title-format 'ambrevar/emms-browser-track-artist-and-title-format)

(provide 'init-emms)
