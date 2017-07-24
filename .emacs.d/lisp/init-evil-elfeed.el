;;; Evil+Elfeed

(evil-set-initial-state 'elfeed-search-mode 'motion)

(evil-define-key 'motion elfeed-search-mode-map
  (kbd "<return>") 'elfeed-search-show-entry
  "R" 'elfeed-search-fetch
  "S" 'elfeed-search-set-filter
  "o" 'elfeed-search-browse-url
  "O" 'elfeed-play-in-mpv ; Custom function
  "r" 'elfeed-search-update--force
  "q" 'quit-window
  "s" 'elfeed-search-live-filter
  "y" 'elfeed-search-yank)

(evil-define-key '(normal visual) elfeed-search-mode-map
  "+" 'elfeed-search-tag-all
  "-" 'elfeed-search-untag-all
  "U" 'elfeed-search-tag-all-unread
  "u" 'elfeed-search-untag-all-unread)

(evil-define-key 'normal elfeed-show-mode-map
  "+" 'elfeed-show-tag
  "-" 'elfeed-show-untag
  "A" 'elfeed-show-add-enclosure-to-playlist
  "P" 'elfeed-show-play-enclosure
  "o" 'elfeed-show-visit
  "O" 'elfeed-play-in-mpv ; Custom function
  "d" 'elfeed-show-save-enclosure
  "r" 'elfeed-show-refresh
  "]" 'elfeed-show-next
  "[" 'elfeed-show-prev
  "\M-j" 'elfeed-show-next
  "\M-k" 'elfeed-show-prev
  "q" 'elfeed-kill-buffer
  "s" 'elfeed-show-new-live-search
  "y" 'elfeed-show-yank)

(provide 'init-evil-elfeed)
