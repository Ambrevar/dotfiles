;;; Evil+pdftools

(defun evil-pdf-view-goto-page (&optional page)
  (interactive "P")
  (if page
      (pdf-view-goto-page page)
    (pdf-view-last-page)))

(defun evil-pdf-view-goto-first-page (&optional page)
  (interactive "P")
  (if page
      (pdf-view-goto-page page)
    (pdf-view-first-page)))

(evil-set-initial-state 'pdf-view-mode 'motion)
(evil-define-key 'motion pdf-view-mode-map
  (kbd "<return>") 'image-next-line
  "j" 'pdf-view-next-line-or-next-page
  "k" 'pdf-view-previous-line-or-previous-page
  (kbd "SPC") 'pdf-view-scroll-up-or-next-page
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
  (kbd "C-j") 'pdf-view-next-page-command
  (kbd "C-k") 'pdf-view-previous-page-command
  (kbd "M-j") 'pdf-view-next-page-command ; Custom
  (kbd "M-k") 'pdf-view-previous-page-command ; Custom
  "q" 'quit-window
  "r" 'revert-buffer
  (kbd "<delete>") 'pdf-view-scroll-down-or-previous-page
  (kbd "S-SPC") 'pdf-view-scroll-down-or-previous-page
  (kbd "<C-down-mouse-1>") 'pdf-view-mouse-extend-region
  (kbd "<M-down-mouse-1>") 'pdf-view-mouse-set-region-rectangle
  (kbd "<down>") 'pdf-view-next-line-or-next-page
  (kbd "<down-mouse-1>")  'pdf-view-mouse-set-region
  (kbd "<next>") 'forward-page
  (kbd "<prior>") 'backward-page
  (kbd "<up>") 'pdf-view-previous-line-or-previous-page
  (kbd "C-c C-c") 'docview-mode
  "zd" 'pdf-view-dark-minor-mode
  (kbd "C-c <tab>") 'pdf-view-extract-region-image

  "sb" 'pdf-view-set-slice-from-bounding-box
  "sm" 'pdf-view-set-slice-using-mouse
  "sr" 'pdf-view-reset-slice

  "gg" 'evil-pdf-view-goto-first-page
  "G" 'evil-pdf-view-goto-page
  "gl" 'pdf-view-goto-label

  "y" 'pdf-view-kill-ring-save
  "h" 'image-backward-hscroll
  "^" 'image-bol
  "$" 'image-eol
  "l" 'image-forward-hscroll
  (kbd "C-f") 'pdf-view-scroll-up-or-next-page
  (kbd "C-b") 'pdf-view-scroll-down-or-previous-page

  "a+" 'image-increase-speed
  "a-" 'image-decrease-speed
  "a0" 'image-reset-speed
  "ar" 'image-reverse-speed

  (kbd "M-s o") 'pdf-occur ; Custom

  "zm" 'pdf-view-midnight-minor-mode
  "zp" 'pdf-view-printer-minor-mode)

(evil-set-initial-state 'pdf-outline-buffer-mode 'motion)
(evil-define-key 'motion pdf-outline-buffer-mode-map
  (kbd "<return>") 'pdf-outline-follow-link
  (kbd "M-<return>") 'pdf-outline-follow-link-and-quit
  (kbd "SPC") 'pdf-outline-display-link
  "." 'pdf-outline-move-to-current-page
  "G" 'pdf-outline-end-of-buffer
  "o" 'pdf-outline-select-pdf-window
  "<" 'pdf-outline-up-heading
  "^" 'pdf-outline-up-heading
  "zf" 'pdf-outline-follow-mode
  (kbd "C-w q") 'pdf-outline-quit-and-kill)

(evil-define-key 'motion pdf-occur-buffer-mode-map
  (kbd "<return>") 'pdf-occur-goto-occurrence
  (kbd "C-o") 'pdf-occur-view-occurrence
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
  (kbd "<delete>") 'tablist-unmark-backward
  (kbd "S-SPC") 'scroll-down-command
  (kbd "<backtab>") 'tablist-backward-column
  (kbd "C-c C-e") 'tablist-export-csv

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
