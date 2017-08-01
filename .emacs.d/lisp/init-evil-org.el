;; Remap org-mode meta keys for convenience
;; - org-evil: Not as polished as of May 2017.
;; - evil-org: Depends on MELPA's org-mode, too big a dependency for me.
;; See https://github.com/Somelauw/evil-org-mode/blob/master/doc/keythemes.org for inspiration.

;; TODO: Agenda keys are not right, M-j is shadowed.

(evil-define-key 'normal org-mode-map
  (kbd "M-<return>") (lambda () (interactive) (evil-insert 1) (org-meta-return))
  "L" 'org-shiftright
  "H" 'org-shiftleft
  "K" 'org-shiftup
  "J" 'org-shiftdown
  "\M-l" 'org-metaright
  "\M-h" 'org-metaleft
  "\M-k" 'org-metaup
  "\M-j" 'org-metadown
  "\M-L" 'org-shiftmetaright
  "\M-H" 'org-shiftmetaleft
  "\M-K" 'org-shiftmetaup
  "\M-J" 'org-shiftmetadown
  "<" 'org-up-element)

(provide 'init-evil-org)
