;;; Evil+Transmission

(evil-set-initial-state 'transmission-mode 'motion)
(evil-define-key 'motion transmission-mode-map
  (kbd "<return>") 'transmission-files
  "D" 'transmission-delete
  "S" 'tabulated-list-sort
  "a" 'transmission-add
  "d" 'transmission-set-download
  "e" 'transmission-peers
  "i" 'transmission-info
  "U" 'transmission-set-ratio
  "x" 'transmission-move
  "q" 'transmission-quit
  "r" 'transmission-remove
  "s" 'transmission-toggle
  "I" 'transmission-trackers-add
  "u" 'transmission-set-upload
  "c" 'transmission-verify
  "C" 'transmission-set-bandwidth-priority)

(evil-set-initial-state 'transmission-files-mode 'motion)
(evil-define-key 'motion transmission-files-mode-map
  (kbd "<return>") 'transmission-find-file
  "\M-l" 'transmission-display-file
  "!" 'transmission-files-command
  "S" 'tabulated-list-sort
  "A" 'transmission-browse-url-of-file
  "X" 'transmission-files-command
  "^" 'quit-window
  "e" 'transmission-peers
  "i" 'transmission-info
  "x" 'transmission-move
  "o" 'transmission-find-file-other-window
  "q" 'quit-window
  "u" 'transmission-files-unwant
  "O" 'transmission-view-file
  "U" 'transmission-files-want
  "C" 'transmission-files-priority)

(evil-set-initial-state 'transmission-info-mode 'motion)
(evil-define-key 'motion transmission-info-mode-map
  "r" 'transmission-trackers-remove
  "c" 'transmission-copy-magnet
  "d" 'transmission-set-torrent-download
  "U" 'transmission-set-torrent-ratio
  "q" 'quit-window
  "a" 'transmission-trackers-add
  "u" 'transmission-set-torrent-upload
  "e" 'transmission-peers
  "x" 'transmission-move
  "I" 'transmission-trackers-add
  "C" 'transmission-set-bandwidth-priority)

(evil-set-initial-state 'transmission-peers-mode 'motion)
(evil-define-key 'motion transmission-peers-mode-map
  "S" 'tabulated-list-sort
  "i" 'transmission-info
  "q" 'quit-window)

(provide 'init-evil-transmission)
