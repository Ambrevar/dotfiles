;;; Evil+Debugger

(evil-set-initial-state 'debugger-mode 'motion)

(evil-define-key 'motion debugger-mode-map
  (kbd "TAB") 'forward-button
  (kbd "RET") 'debug-help-follow
  (kbd "SPC") 'next-line
  "R" 'debugger-record-expression
  "gb" 'debugger-frame
  "c" 'debugger-continue
  "d" 'debugger-step-through
  "x" 'debugger-eval-expression
  "J" 'debugger-jump
  "zl" 'debugger-list-functions
  "q" 'top-level
  "r" 'debugger-return-value
  "u" 'debugger-frame-clear
  "p" 'debugger-toggle-locals)

(provide 'init-evil-debugger)
