;;; Evil+Calendar

(evil-define-key 'motion calendar-mode-map
  "v" 'calendar-set-mark
  "h" 'calendar-backward-day
  "0" 'calendar-beginning-of-week
  "$" 'calendar-end-of-week
  "l" 'calendar-forward-day
  "j" 'calendar-forward-week
  "k" 'calendar-backward-week
  "\C-f" 'calendar-scroll-left-three-months
  ;; (kbd "<space>") 'scroll-other-window
  "." 'calendar-goto-today
  "<" 'calendar-scroll-right
  ">" 'calendar-scroll-left
  "?" 'calendar-goto-info-node
  "D" 'diary-view-other-diary-entries
  "M" 'calendar-lunar-phases
  "S" 'calendar-sunrise-sunset
  "a" 'calendar-list-holidays
  "c" 'org-calendar-goto-agenda
  "d" 'diary-view-entries
  "\M-h" 'calendar-cursor-holidays
  "m" 'diary-mark-entries
  "o" 'calendar-other-month
  "q" 'calendar-exit
  "s" 'diary-show-all-entries
  "u" 'calendar-unmark
  "x" 'calendar-mark-holidays
  "\C-c\C-l" 'calendar-redraw
  "[" 'calendar-backward-year
  "]" 'calendar-forward-year
  "\M-<" 'calendar-beginning-of-year
  "\M-=" 'calendar-count-days-region
  "\M->" 'calendar-end-of-year
  "(" 'calendar-beginning-of-month
  ")" 'calendar-end-of-month
  "\C-b" 'calendar-scroll-right-three-months
  "{" 'calendar-backward-month
  "}" 'calendar-forward-month)

(provide 'init-evil-calendar)
