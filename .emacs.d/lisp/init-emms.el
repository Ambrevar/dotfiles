;;; Emms

(setq emms-directory (concat emacs-cache-folder "emms"))
(emms-all)
(emms-history-load)
(emms-default-players)
(when (require 'emms-player-mpv nil t)
  ;; Don't use default players as they have poor playback support, e.g. seeking is missing for OGG.
  ;; REVIEW: mpv should not display covers.
  ;; Reported at https://github.com/dochang/emms-player-mpv/issues/8.
  (setq emms-player-list '(emms-player-mpv emms-player-mplayer-playlist emms-player-mplayer)))

(setq emms-source-file-default-directory "~/music/"
      emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find)

(add-to-list 'emms-info-functions 'emms-info-cueinfo)
(when (executable-find "emms-print-metadata")
  (require 'emms-info-libtag)
  (add-to-list 'emms-info-functions 'emms-info-libtag)
  (delete 'emms-info-ogginfo emms-info-functions)
  (delete 'emms-info-mp3info emms-info-functions))

;;; TODO: Cache covers thumbnails.
;;; https://emacs.stackexchange.com/questions/3667/what-are-the-options-to-play-music-from-within-emacs
;;; `emms-browser-covers' takes a function, so it's possible to build a cache of covers in
;;; Do not forget to compare dates.
;; (setq emms-browser-default-covers (list "" nil nil))

;;; TODO: Resume on restart.
;;; See `emms-bookmark-add' and `emms-bookmark-previous'.

;;; TODO: See if mpd is faster at building the db. Not so important.
;;; TODO: Delete entry from cache? See `emms-cache-del'. Not so important.

;;; TODO: Browse by album-artist? libtag has the field.
;; https://emacs.stackexchange.com/questions/10412/sort-by-artist-in-emms-with-compilation-albums/10435

;;; TODO: Change face from purple to white?

(provide 'init-emms)
