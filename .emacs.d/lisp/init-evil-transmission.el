;;; Evil+Transmission

(evil-set-initial-state 'transmission-mode 'motion)
(evil-define-key 'motion transmission-mode-map
  ;; motion
  (kbd "SPC") 'scroll-up-command
  (kbd "S-SPC") 'scroll-down-command
  (kbd "<delete>") 'scroll-down-command

  ;; sort
  "s" 'tabulated-list-sort

  (kbd "<return>") 'transmission-files
  "p" 'transmission-peers
  "i" 'transmission-info

  "a" 'transmission-add
  ;; "D" 'transmission-delete ; Useless with `transmission-remove'?
  "p" 'transmission-move
  "d" 'transmission-remove
  "x" 'transmission-toggle ; EMMS has "x" for pause.
  "t" 'transmission-trackers-add
  "c" 'transmission-verify ; "c" for "[c]heck".
  "D" 'transmission-set-download
  "U" 'transmission-set-upload
  "S" 'transmission-set-ratio ; "S" for "[S]eed"
  "P" 'transmission-set-bandwidth-priority

  ;; mark
  "m" 'transmission-toggle-mark
  "M" 'transmission-unmark-all
  "~" 'transmission-invert-marks

  ;; update
  "gr" 'revert-buffer

  ;; quit
  "q" 'transmission-quit
  "ZQ" 'evil-quit
  "ZZ" 'transmission-quit)

(evil-set-initial-state 'transmission-files-mode 'motion)
(evil-define-key 'motion transmission-files-mode-map
  (kbd "SPC") 'scroll-up-command
  (kbd "S-SPC") 'scroll-down-command
  (kbd "<delete>") 'scroll-down-command

  ;; sort
  "s" 'tabulated-list-sort

  "p" 'transmission-peers
  "i" 'transmission-info

  "R" 'transmission-move
  "u" 'transmission-files-unwant
  "U" 'transmission-files-want
  "P" 'transmission-files-priority
  (kbd "<return>") 'transmission-find-file
  (kbd "S-<return>") 'transmission-display-file
  "o" 'transmission-find-file-other-window
  "O" 'transmission-view-file
  "!" 'transmission-files-command
  ;; "X" 'transmission-files-command
  "t" 'transmission-trackers-add
  "T" 'transmission-trackers-remove

  ;; goto URL
  "gu" 'transmission-browse-url-of-file ; See mu4e.

  ;; quit
  "q" 'transmission-quit
  "ZQ" 'evil-quit
  "ZZ" 'transmission-quit)

(evil-set-initial-state 'transmission-info-mode 'motion)
(evil-define-key 'motion transmission-info-mode-map
  "p" 'transmission-peers

  "t" 'transmission-trackers-add
  "T" 'transmission-trackers-remove
  "D" 'transmission-set-torrent-download
  "U" 'transmission-set-torrent-upload
  "S" 'transmission-set-torrent-ratio ; "S" for "[S]eed"
  "P" 'transmission-set-bandwidth-priority
  "gy" 'transmission-copy-magnet ; TODO: Use "ym"?
  "R" 'transmission-move

  ;; quit
  "q" 'quit-window
  "ZQ" 'evil-quit
  "ZZ" 'quit-window)

(evil-set-initial-state 'transmission-peers-mode 'motion)
(evil-define-key 'motion transmission-peers-mode-map
  ;; sort
  "s" 'tabulated-list-sort

  "i" 'transmission-info

  ;; quit
  "q" 'quit-window
  "ZQ" 'evil-quit
  "ZZ" 'quit-window)

(provide 'init-evil-transmission)
