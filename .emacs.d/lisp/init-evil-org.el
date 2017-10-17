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
  (kbd "M-h") 'org-metaleft
  (kbd "M-j") 'org-metadown
  (kbd "M-k") 'org-metaup
  (kbd "M-l") 'org-metaright
  (kbd "M-H") 'org-shiftmetaleft
  (kbd "M-J") 'org-shiftmetadown
  (kbd "M-K") 'org-shiftmetaup
  (kbd "M-L") 'org-shiftmetaright
  "<" 'org-up-element)

;; Horizontal movements have little use, so we override "f" and "t", ";" and
;; ",", "|".  We keep "w", "b", "e", "ge" and the upcase versions.
(evil-set-initial-state 'org-agenda-mode 'motion)
(evil-define-key 'motion org-agenda-mode-map
  ;; TODO: Bind date selection from miniprompt to S-<hjkl>.
  ;; TODO: Unused keys: D, x, X, o, p, P

  (kbd "<tab>") 'org-agenda-goto
  (kbd "<return>") 'org-agenda-switch-to
  (kbd "S-<return>") 'org-agenda-recenter

  (kbd "<space>") 'org-agenda-show-and-scroll-up
  (kbd "<delete>") 'org-agenda-show-scroll-down
  (kbd "<backspace>") 'org-agenda-show-scroll-down

  ;; Motion
  "j" 'org-agenda-next-line
  "k" 'org-agenda-previous-line
  (kbd "]") 'org-agenda-earlier
  (kbd "[") 'org-agenda-later
  (kbd "C-k") 'org-agenda-previous-item
  (kbd "C-j") 'org-agenda-next-item

  ;; Manipulation
  ;; We follow standard org-mode bindings (not org-agenda bindings):
  ;; <HJKL> change todo items and priorities.
  ;; M-<jk> drag lines.
  ;; M-<hl> cannot demote/promote, we use it for "do-date".
  "H" 'org-agenda-todo-previousset
  "L" 'org-agenda-todo-nextset
  "J" 'org-agenda-priority-down
  "K" 'org-agenda-priority-up
  (kbd "M-j") 'org-agenda-drag-line-forward
  (kbd "M-k") 'org-agenda-drag-line-backward
  (kbd "M-h") 'org-agenda-do-date-earlier
  (kbd "M-l") 'org-agenda-do-date-later

  ;; Undo
  "u" 'org-agenda-undo
  "U" 'org-agenda-redo-all
  (kbd "C-r") 'org-agenda-redo

  ;; Actions
  "dd" 'org-agenda-kill
  "dA" 'org-agenda-archive
  "da" 'org-agenda-archive-default-with-confirmation
  "st" 'org-agenda-set-tags
  "se" 'org-agenda-set-effort
  "S" 'org-timer-set-timer
  "i" 'org-agenda-diary-entry
  "a" 'org-agenda-add-note
  "A" 'org-agenda-append-agenda
  "C" 'org-agenda-capture

  ;; Marking
  "mm" 'org-agenda-bulk-toggle
  "mM" 'org-agenda-bulk-toggle-all
  "m%" 'org-agenda-bulk-mark-regexp
  "mu" 'org-agenda-bulk-remove-all-marks
  "ma" 'org-agenda-bulk-action

  ;; Quit
  "ZQ" 'org-agenda-exit
  "ZZ" 'org-agenda-quit

  ;; Display
  ;; "Dispatch" can prefix the following:
  ;; 'org-agenda-toggle-deadlines
  ;; 'org-agenda-toggle-diary
  ;; 'org-agenda-follow-mode
  ;; 'org-agenda-log-mode
  ;; 'org-agenda-entry-text-mode
  ;; 'org-agenda-toggle-time-grid
  ;; 'org-agenda-day-view
  ;; 'org-agenda-week-view
  ;; 'org-agenda-year-view
  "z" 'org-agenda-view-mode-dispatch
  "ZD" 'org-agenda-dim-blocked-tasks

  ;; Filter
  "fc" 'org-agenda-filter-by-category
  "fr" 'org-agenda-filter-by-regexp
  "fe" 'org-agenda-filter-by-effort
  "ft" 'org-agenda-filter-by-tag
  "fu" 'org-agenda-filter-remove-all
  "f^" 'org-agenda-filter-by-top-headline
  "~" 'org-agenda-limit-interactively

  ;; Clock
  "I" 'org-agenda-clock-in
  "O" 'org-agenda-clock-out
  "cg" 'org-agenda-clock-goto
  "cc" 'org-agenda-clock-cancel
  "cr" 'org-agenda-clockreport-mode

  ;; Go and show
  "." 'org-agenda-goto-today ; TODO: What about evil-repeat?
  "gc" 'org-agenda-goto-calendar
  "gC" 'org-agenda-convert-date
  "gd" 'org-agenda-goto-date
  "gh" 'org-agenda-holidays
  "gm" 'org-agenda-phases-of-moon
  "gs" 'org-agenda-sunrise-sunset
  "gt" 'org-agenda-show-tags

  ;; Others
  "+" 'org-agenda-manipulate-query-add
  "-" 'org-agenda-manipulate-query-subtract
  ;; TODO: Work out the following.
  ;; 'org-agenda-date-prompt
  ;; 'org-agenda-show-the-flagging-note
  ;; 'org-save-all-org-buffers
  )

(provide 'init-evil-org)
