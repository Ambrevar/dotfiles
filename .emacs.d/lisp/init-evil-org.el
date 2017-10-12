;;; Evil+Org
;;; Remap org-mode meta keys for convenience
;;; - org-evil: Not as polished as of May 2017.
;;; - evil-org: Depends on MELPA's org-mode, too big a dependency for me.
;;; See https://github.com/Somelauw/evil-org-mode/blob/master/doc/keythemes.org for inspiration.

;; TODO: M-j is shadowed.  Also set calendar movements.
;;; See https://github.com/Somelauw/evil-org-mode/issues/15.

(evil-define-key 'normal org-mode-map
  (kbd "M-<return>") (lambda () (interactive) (evil-insert 1) (org-meta-return))
  "H" 'org-shiftleft
  "J" 'org-shiftdown
  "K" 'org-shiftup
  "L" 'org-shiftright
  "\M-h" 'org-metaleft
  "\M-j" 'org-metadown
  "\M-k" 'org-metaup
  "\M-l" 'org-metaright
  "\M-H" 'org-shiftmetaleft
  "\M-J" 'org-shiftmetadown
  "\M-K" 'org-shiftmetaup
  "\M-L" 'org-shiftmetaright
  "<" 'org-up-element)

(evil-set-initial-state 'org-agenda-mode 'motion)
(evil-define-key 'motion org-agenda-mode-map
  (kbd "TAB") 'org-agenda-goto
  (kbd "RET") 'org-agenda-switch-to
  (kbd "SPC") 'org-agenda-show-and-scroll-up
  (kbd "DEL") 'org-agenda-show-scroll-down
  (kbd "<backspace>") 'org-agenda-show-scroll-down
  "dd" 'org-agenda-kill
  "u" 'org-agenda-undo
  "!" 'org-agenda-toggle-deadlines
  "zd" 'org-agenda-dim-blocked-tasks
  "za" 'org-agenda-archive
  "zr" 'org-agenda-bulk-mark-regexp
  "zm" 'org-agenda-bulk-mark-all
  "+" 'org-agenda-priority-up
  "-" 'org-agenda-priority-down
  ;; "," 'org-agenda-priority
  "." 'org-agenda-goto-today
  "\M-sf." 'org-agenda-filter-by-tag
  "s" 'org-agenda-set-tags
  "S" 'org-agenda-set-timer
  "<" 'org-agenda-filter-by-category
  "=" 'org-agenda-filter-by-regexp
  ">" 'org-agenda-date-prompt
  "zf" 'org-agenda-show-the-flagging-note
  "A" 'org-agenda-append-agenda
  "zb" 'org-agenda-bulk-action
  "C" 'org-agenda-convert-date
  "D" 'org-agenda-toggle-diary
  "ge" 'org-agenda-entry-text-mode
  "zf" 'org-agenda-follow-mode
  "zg" 'org-agenda-toggle-time-grid
  "gh" 'org-agenda-holidays
  "I" 'org-agenda-clock-in
  "O" 'org-agenda-clock-out
  "J" 'org-agenda-clock-goto
  ;; "L" 'org-agenda-recenter
  "gm" 'org-agenda-phases-of-moon
  "\C-j" 'org-agenda-next-item
  "\C-k" 'org-agenda-previous-item
  ;; "Q" 'org-agenda-quit
  "q" 'org-agenda-quit
  "R" 'org-agenda-clockreport-mode
  "gs" 'org-agenda-sunrise-sunset
  "gi" 'org-agenda-show-tags
  "zM" 'org-agenda-bulk-unmark-all
  "X" 'org-agenda-clock-cancel
  "[" 'org-agenda-manipulate-query-add
  "]" 'org-agenda-manipulate-query-subtract
  "z^" 'org-agenda-filter-by-top-headline
  "z_" 'org-agenda-filter-by-effort
  "a" 'org-agenda-archive-default-with-confirmation
  "H" 'org-agenda-earlier
  "L" 'org-agenda-later
  "c" 'org-agenda-goto-calendar
  ;; "d" 'org-agenda-day-view
  ;; "se" 'org-agenda-set-effort
  "gr" 'org-agenda-redo-all
  "\C-r" 'org-agenda-redo
  "i" 'org-agenda-diary-entry
  "gd" 'org-agenda-goto-date
  "zk" 'org-agenda-capture
  "zl" 'org-agenda-log-mode
  ;; "zm" 'org-agenda-bulk-mark
  "j" 'org-agenda-next-line
  "k" 'org-agenda-previous-line
  ;; "s" 'org-save-all-org-buffers
  "gt" 'org-agenda-todo
  ;; "zM" 'org-agenda-bulk-unmark
  "zv" 'org-agenda-view-mode-dispatch
  "gw" 'org-agenda-week-view
  "Q" 'org-agenda-exit
  "gy" 'org-agenda-year-view
  ;; "za" 'org-agenda-add-note
  "{" 'org-agenda-manipulate-query-add
  "|" 'org-agenda-filter-remove-all ; goto-column?
  "}" 'org-agenda-manipulate-query-subtract
  "~" 'org-agenda-limit-interactively ; invert-char?
  "H" 'org-agenda-do-date-earlier
  "J" 'org-agenda-priority-down
  "K" 'org-agenda-priority-up
  "L" 'org-agenda-do-date-later
  "\M-H" 'org-agenda-todo-previousset
  "\M-J" 'org-agenda-drag-line-forward
  "\M-K" 'org-agenda-drag-line-backward
  "\M-L" 'org-agenda-todo-nextset)

(provide 'init-evil-org)
