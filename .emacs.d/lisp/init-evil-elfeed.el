;;; Evil+Elfeed

(evil-set-initial-state 'elfeed-search-mode 'motion)
(evil-define-key 'motion elfeed-search-mode-map
  (kbd "<return>") 'elfeed-search-show-entry
  "o" 'elfeed-search-browse-url
  "y" 'elfeed-search-yank

  ;; filter
  "s" 'elfeed-search-live-filter
  "S" 'elfeed-search-set-filter

  ;; update
  "R" 'elfeed-search-fetch ; TODO: Which update function is more useful?
  "r" 'elfeed-search-update--force

  ;; quit
  "q" 'quit-window
  "ZQ" 'quit-window
  "ZZ" 'quit-window)

(evil-define-key '(motion visual) elfeed-search-mode-map
  "+" 'elfeed-search-tag-all
  "-" 'elfeed-search-untag-all
  "U" 'elfeed-search-tag-all-unread
  "u" 'elfeed-search-untag-all-unread)

(evil-set-initial-state 'elfeed-show-mode 'motion)
(evil-define-key 'motion elfeed-show-mode-map
  "o" 'elfeed-show-visit

  ;; filter
  "s" 'elfeed-show-new-live-search

  "y" 'elfeed-show-yank

  "+" 'elfeed-show-tag
  "-" 'elfeed-show-untag

  "A" 'elfeed-show-add-enclosure-to-playlist
  "P" 'elfeed-show-play-enclosure
  "d" 'elfeed-show-save-enclosure

  "]" 'elfeed-show-next
  "[" 'elfeed-show-prev
  (kbd "C-j") 'elfeed-show-next
  (kbd "C-k") 'elfeed-show-prev

  ;; update
  "r" 'elfeed-show-refresh

  ;; quit
  "q" 'elfeed-kill-buffer
  "ZQ" 'elfeed-kill-buffer
  "ZZ" 'elfeed-kill-buffer)

;;; Custom
(evil-define-key 'motion elfeed-search-mode-map
  (kbd "<return>") 'elfeed-visit-or-play-with-mpv
  "o" 'elfeed-visit-or-play-with-mpv)
(evil-define-key 'motion elfeed-show-mode-map
  (kbd "<return>") 'elfeed-visit-or-play-with-mpv
  "o" 'elfeed-visit-or-play-with-mpv)

(provide 'init-evil-elfeed)
