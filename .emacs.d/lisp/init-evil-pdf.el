;;; Evil+pdftools

;;; TODO: Add other modes? Looks like it's not needed.
(evil-set-initial-state 'pdf-view-mode 'motion)

(evil-define-key 'motion pdf-view-mode-map
  (kbd "<return>") 'image-next-line
  "j" 'pdf-view-next-line-or-next-page
  "k" 'pdf-view-previous-line-or-previous-page
  (kbd "<space>") 'pdf-view-scroll-up-or-next-page
  "'" 'pdf-view-jump-to-register
  "+" 'pdf-view-enlarge
  "-" 'pdf-view-shrink
  "0" 'pdf-view-scale-reset
  "=" 'pdf-view-enlarge
  "F" 'image-goto-frame
  "H" 'pdf-view-fit-height-to-window
  "P" 'pdf-view-fit-page-to-window
  "Q" 'kill-this-buffer
  "W" 'pdf-view-fit-width-to-window
  "b" 'image-previous-frame
  "f" 'image-next-frame
  "m" 'pdf-view-position-to-register
  "\C-j" 'pdf-view-next-page-command
  "\C-k" 'pdf-view-previous-page-command
  "\M-j" 'pdf-view-next-page-command ; Custom
  "\M-k" 'pdf-view-previous-page-command ; Custom
  "q" 'quit-window
  "r" 'revert-buffer
  (kbd "DEL") 'pdf-view-scroll-down-or-previous-page
  (kbd "S-SPC") 'pdf-view-scroll-down-or-previous-page
  (kbd "<C-down-mouse-1>") 'pdf-view-mouse-extend-region
  (kbd "<M-down-mouse-1>") 'pdf-view-mouse-set-region-rectangle
  (kbd "<down>") 'pdf-view-next-line-or-next-page
  (kbd "<down-mouse-1>")  'pdf-view-mouse-set-region
  (kbd "<next>") 'forward-page
  (kbd "<prior>") 'backward-page
  (kbd "<up>") 'pdf-view-previous-line-or-previous-page
  "\C-c\C-c" 'docview-mode
  "zd" 'pdf-view-dark-minor-mode
  (kbd "C-c TAB") 'pdf-view-extract-region-image

  "sb" 'pdf-view-set-slice-from-bounding-box
  "sm" 'pdf-view-set-slice-using-mouse
  "sr" 'pdf-view-reset-slice

  "gg" 'pdf-view-first-page
  "G" 'pdf-view-last-page
  "gl" 'pdf-view-goto-label

  [remap evil-goto-line] 'pdf-view-goto-page
  "y" 'pdf-view-kill-ring-save
  "h" 'image-backward-hscroll
  "^" 'image-bol
  "$" 'image-eol
  "l" 'image-forward-hscroll
  "\C-f" 'pdf-view-scroll-up-or-next-page
  "\C-b" 'pdf-view-scroll-down-or-previous-page

  "a+" 'image-increase-speed
  "a-" 'image-decrease-speed
  "a0" 'image-reset-speed
  "ar" 'image-reverse-speed

  "\M-so" 'pdf-occur ; Custom

  "zm" 'pdf-view-midnight-minor-mode
  "zp" 'pdf-view-printer-minor-mode)

(evil-define-key 'motion pdf-outline-minor-mode-map "o" 'pdf-outline)

(evil-define-key 'motion pdf-occur-buffer-mode-map
  (kbd "RET") 'pdf-occur-goto-occurrence
  "\C-o" 'pdf-occur-view-occurrence
  (kbd "SPC") 'pdf-occur-view-occurrence
  "A" 'pdf-occur-tablist-gather-documents
  "D" 'pdf-occur-tablist-do-delete
  "G" 'tablist-revert
  "K" 'pdf-occur-abost-search
  "S" 'tabulated-list-sort
  "U" 'tablist-unmark-all-marks
  "a" 'tablist-flag-forward
  ;; "f" 'tablist-find-entry ; TODO: Equivalent to 'pdf-occur-goto-occurrence?
  "r" 'pdf-occur-revert-buffer-with-args
  "d" 'tablist-do-kill-lines
  "m" 'tablist-mark-forward
  "q" 'tablist-quit
  "s" 'tablist-sort
  "t" 'tablist-toggle-marks
  "u" 'tablist-unmark-forward
  "x" 'pdf-occur-tablist-do-flagged-delete
  (kbd "DEL") 'tablist-unmark-backward
  (kbd "S-SPC") 'scroll-down-command
  (kbd "<backtab>") 'tablist-backward-column
  "\C-c\C-e" 'tablist-export-csv

  [remap evil-first-non-blank] 'tablist-move-to-major-columnj
  [remap evil-next-line] 'tablist-next-line
  [remap evil-previous-line] 'tablist-previous-line

  "/!" 'tablist-negate-filter
  "//" 'tablist-display-filter
  "/=" 'tablist-push-equal-filter
  "/C" 'tablist-clear-filter
  "/D" 'tablist-delete-named-filter
  "/a" 'tablist-push-named-filter
  "/d" 'tablist-deconstruct-named-filter
  "/e" 'tablist-edit-filter
  "/n" 'tablist-push-numeric-filter
  "/p" 'tablist-pop-filter
  "/r" 'tablist-push-regexp-filter
  "/s" 'tablist-name-current-filter
  "/t" 'tablist-toggle-first-filter-logic
  "/z" 'tablist-suspend-filter

  "*!" 'tablist-unmark-all-marks
  "*c" 'tablist-change-marks
  "*m" 'tablist-mark-forward
  "*n" 'tablist-mark-items-numeric
  "*r" 'tablist-mark-items-regexp

  "%m"  'tablist-mark-items-regexp)

(provide 'init-evil-pdf)
