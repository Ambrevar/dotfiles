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
  (kbd "s") 'debbugs-gnu-narrow-to-status
  ;; "S" 'debbugs-gnu-widen ; Useless if we can just press "s RET" (empty filter).
  "S" 'debbugs-gnu-toggle-suppress
  "r" 'debbugs-gnu-show-all-blocking-reports

  ;; sorting
  "o" 'debbugs-gnu-toggle-sort
  "O" 'tabulated-list-sort

  ;; show
  "gB" 'debbugs-gnu-show-blocking-reports
  "gb" 'debbugs-gnu-show-blocked-by-reports

  ;; marking
  "m" 'debbugs-gnu-toggle-tag

  ;; update
  "gr" 'debbugs-gnu-rescan

  ;; quit
  "q" 'quit-window
  "ZQ" 'quit-window
  "ZZ" 'quit-window)

(provide 'init-evil-debbugs)
