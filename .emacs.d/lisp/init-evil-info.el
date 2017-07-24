;;; Evil+Info

(evil-define-key 'motion Info-mode-map
  (kbd "C-SPC") 'Info-scroll-up ; Custom
  "\C-j" 'Info-next
  "\C-k" 'Info-prev
  "\M-j" 'Info-next ; Custom
  "\M-k" 'Info-prev ; Custom
  "\M-sf" 'Info-goto-node
  "w" 'evil-forward-word-begin
  "e" 'evil-forward-word-end
  "ge" 'evil-backward-word-end
  "gE" 'evil-backward-WORD-end
  "b" 'evil-backward-word-begin
  "\M-w" 'Info-copy-current-node-name
  "gg" 'evil-goto-first-line
  "gt" 'Info-top-node
  "gT" 'Info-toc
  "gf" 'Info-follow-reference
  "t" 'evil-find-char-to
  "T" 'evil-find-char-to-backward
  "f" 'evil-find-char
  "n" 'evil-search-next
  "?" 'evil-search-backward
  "p" nil)

(provide 'init-evil-info)
