;;; Emms

;;; TODO: See if mpd is faster at building the db. Not so important.
;;; TODO: Change face from purple to white?
;;; TODO: emms-all causes some "require"d files to be loaded twice if called after, say, emms-browser was loaded.
(emms-all)
(emms-history-load)

(setq emms-player-list (list emms-player-mpv)
      emms-source-file-default-directory (expand-file-name "~/music")
      emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find
      ;; Cover thumbnails.
      emms-browser-covers 'emms-browser-cache-thumbnail)
(add-to-list 'emms-player-mpv-parameters "--no-audio-display")
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
           ;; TODO: Adapt this to simple-mpv.
           (call-process-shell-command (emms-player-mpv--format-command "mute") nil nil nil))
      (emms-pause))))

(when (require 'helm-emms nil t)
  (setq helm-emms-default-sources
        '(helm-source-emms-dired
          helm-source-emms-files ; Disable for a huge speed-up.
          helm-source-emms-streams)))

(defun ambrevar/emms-play-on-add (old-pos)
  "Play tracks when calling `emms-browser-add-tracks' if nothing
is currently playing."
  (interactive)
  (when (or (not emms-player-playing-p)
            emms-player-paused-p
            emms-player-stopped-p)
    (with-current-emms-playlist
      (goto-char old-pos)
      ;; if we're sitting on a group name, move forward
      (unless (emms-playlist-track-at (point))
        (emms-playlist-next))
      (emms-playlist-select (point)))
    (emms-stop)
    (emms-start)))
(add-hook 'emms-browser-tracks-added-hook 'ambrevar/emms-play-on-add)

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
