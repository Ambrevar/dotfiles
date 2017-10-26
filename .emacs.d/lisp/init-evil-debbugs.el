;;; Evil+Debbugs

(evil-set-initial-state 'debbugs-gnu-mode 'motion)

(evil-define-key 'motion debbugs-gnu-mode-map
  ;; motion
  (kbd "<tab>") 'forward-button
  (kbd "<backtab>") 'backward-button
  (kbd "SPC") 'scroll-up-command

  (kbd "<return>") 'debbugs-gnu-select-report
  "c" 'debbugs-gnu-send-control-message
  "d" 'debbugs-gnu-display-status

  ;; filtering
  (kbd "M-s f") 'debbugs-gnu-narrow-to-status ; TODO: bad binding

  ;; sorting
  "s" 'debbugs-gnu-toggle-sort
  "S" 'tabulated-list-sort

  ;; show
  "gB" 'debbugs-gnu-show-blocking-reports
  "r" 'debbugs-gnu-show-all-blocking-reports
  "gb" 'debbugs-gnu-show-blocked-by-reports
  "i" 'debbugs-gnu-toggle-tag
  "o" 'debbugs-gnu-widen
  "x" 'debbugs-gnu-toggle-suppress ; TODO: bad binding

  ;; update
  "gr" 'debbugs-gnu-rescan

  ;; quit
  "q" 'quit-window
  "ZQ" 'quit-window
  "ZZ" 'quit-window)

(provide 'init-evil-debbugs)
