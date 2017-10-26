;;; Evil+Gnus

(evil-set-initial-state 'gnus-summary-mode 'motion)
(evil-define-key 'motion gnus-summary-mode-map
  ;; motion
  (kbd "<tab>") 'gnus-summary-widget-forward
  (kbd "<backtab>") 'gnus-summary-widget-backward
  (kbd "<delete>") 'gnus-summary-prev-page
  (kbd "S-SPC") 'gnus-summary-prev-page
  (kbd "SPC") 'gnus-summary-next-page
  (kbd "<return>") 'gnus-summary-scroll-up ; TODO: bad binding?
  "]" 'gnus-summary-next-article
  "[" 'gnus-summary-prev-article
  (kbd "C-j") 'gnus-summary-next-article
  (kbd "C-k") 'gnus-summary-prev-article

  "zz" 'gnus-recenter
  "z#" 'gnus-summary-mark-as-processable
  "!" 'gnus-summary-execute-command
  "zc" 'gnus-cache-enter-article
  "gb" 'gnus-summary-best-unread-article
  "gf" 'gnus-summary-first-unread-article
  "z/" 'gnus-summary-limit-map
  ;; "<" "=" ">" ...
  "zd" 'gnus-summary-mark-as-dormant
  ;; "gA"
  ;; "gB"
  ;; "zC"
  ;; "zD"
  ;; "zE"
  ;; "zF"
  ;; "gG"
  ;; "gH"
  ;; "zI"
  ;; "gK"
  ;; "zL"
  ;; "gM"

  "r" 'gnus-summary-reply
  "R" 'gnus-summary-reply-with-original

  "gO" 'gnus-summary-save-map
  "gS" 'gnus-summary-send-map
  "gT" 'gnus-summary-thread-map
  "gV" 'gnus-summary-score-map
  "gW" 'gnus-summary-wash-map
  "X" 'gnus-uu-extract-map
  "gY" 'gnus-summary-buffer-map
  "gZ" 'gnus-summary-exit-map
  "z^" 'gnus-summary-refer-parent-article
  ;; "a" ...
  "zt" 'gnus-summary-toggle-header
  "u" 'gnus-summary-tick-article-forward
  "U" 'gnus-summary-tick-article-backward
  "x" 'gnus-summary-limit-to-unread
  "zp" 'gnus-summary-pipe-output

  ;; filter
  "s" 'gnus-summary-isearch-article

  ;; quit
  "Q" 'gnus-summary-exit-no-update
  "q" 'gnus-summary-exit
  "ZQ" 'gnus-summary-exit-no-update
  "ZZ" 'gnus-summary-exit)

(evil-set-initial-state 'gnus-article-mode 'motion)

(provide 'init-evil-gnus)
