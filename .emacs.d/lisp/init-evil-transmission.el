;;; Evil+Transmission

(evil-set-initial-state 'transmission-mode 'motion)
(evil-define-key 'motion transmission-mode-map
  (kbd "SPC") 'scroll-up-command
  (kbd "S-SPC") 'scroll-down-command
  (kbd "<delete>") 'scroll-down-command

  ;; sort
  "s" 'tabulated-list-sort

  ;; "D" 'transmission-delete ; TODO: useless with `transmission-remove'?
  "a" 'transmission-add
  "p" 'transmission-move
  "d" 'transmission-remove

  (kbd "<return>") 'transmission-files
  "e" 'transmission-peers
  "i" 'transmission-info

  "x" 'transmission-toggle ; TODO: Match EMMS' pause.
  "A" 'transmission-trackers-add
  "c" 'transmission-verify
  "D" 'transmission-set-download
  "U" 'transmission-set-upload
  "R" 'transmission-set-ratio
  "P" 'transmission-set-bandwidth-priority

  ;; mark
  "m" 'transmission-toggle-mark
  "M" 'transmission-unmark-all
  "~" 'transmission-invert-marks

  ;; update
  "gr" 'revert-buffer

  ;; quit
  "q" 'transmission-quit
  "ZQ" 'transmission-quit
  "ZZ" 'transmission-quit)

(evil-set-initial-state 'transmission-files-mode 'motion)
(evil-define-key 'motion transmission-files-mode-map
  (kbd "SPC") 'scroll-up-command
  (kbd "S-SPC") 'scroll-down-command
  (kbd "<delete>") 'scroll-down-command

  ;; sort
  "s" 'tabulated-list-sort

  "e" 'transmission-peers
  "i" 'transmission-info

  (kbd "<return>") 'transmission-find-file
  (kbd "S-<return>") 'transmission-display-file
  "!" 'transmission-files-command
  ;; "X" 'transmission-files-command
  "A" 'transmission-browse-url-of-file
  "o" 'transmission-find-file-other-window
  "O" 'transmission-view-file

  "p" 'transmission-move
  "u" 'transmission-files-unwant
  "U" 'transmission-files-want
  "P" 'transmission-files-priority

  ;; quit
  "q" 'quit-window
  "ZQ" 'transmission-quit
  "ZZ" 'transmission-quit)

(evil-set-initial-state 'transmission-info-mode 'motion)
(evil-define-key 'motion transmission-info-mode-map
  "e" 'transmission-peers

  "A" 'transmission-trackers-add
  "r" 'transmission-trackers-remove ; TODO: Add to the main view as well? Bad binding.

  "D" 'transmission-set-torrent-download
  "U" 'transmission-set-torrent-upload
  "R" 'transmission-set-torrent-ratio
  "P" 'transmission-set-bandwidth-priority

  "c" 'transmission-copy-magnet ; TODO: Should be `transmission-verify' like in the main view?

  "p" 'transmission-move

  ;; quit
  "q" 'quit-window
  "ZQ" 'quit-window
  "ZZ" 'quit-window)

(evil-set-initial-state 'transmission-peers-mode 'motion)
(evil-define-key 'motion transmission-peers-mode-map
  ;; sort
  "s" 'tabulated-list-sort

  "i" 'transmission-info

  ;; quit
  "q" 'quit-window
  "ZQ" 'quit-window
  "ZZ" 'quit-window)

(provide 'init-evil-transmission)
