;;; Evil+Gnus

(evil-set-initial-state 'gnus-summary-mode 'motion)
(evil-define-key 'motion gnus-summary-mode-map
  (kbd "<tab>") 'gnus-summary-widget-forward
  (kbd "<backtab>") 'gnus-summary-widget-backward
  (kbd "<delete>") 'gnus-summary-prev-page
  (kbd "S-<space>") 'gnus-summary-prev-page
  (kbd "<space>") 'gnus-summary-next-page
  (kbd "<return>") 'gnus-summary-scroll-up
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
  "J" 'gnus-summary-next-article
  "K" 'gnus-summary-prev-article
  "gO" 'gnus-summary-save-map
  "Q" 'gnus-summary-exit-no-update
  "R" 'gnus-summary-reply-with-original
  "gS" 'gnus-summary-send-map
  "gT" 'gnus-summary-thread-map
  "gV" 'gnus-summary-score-map
  "gW" 'gnus-summary-wash-map
  "X" 'gnus-uu-extract-map
  "gY" 'gnus-summary-buffer-map
  "gZ" 'gnus-summary-exit-map
  "z^" 'gnus-summary-refer-parent-article
  ;; "a" ...
  "r" 'gnus-summary-reply
  "s" 'gnus-summary-isearch-article
  "zt" 'gnus-summary-toggle-header
  "u" 'gnus-summary-tick-article-forward
  "U" 'gnus-summary-tick-article-backward
  "x" 'gnus-summary-limit-to-unread
  "zp" 'gnus-summary-pipe-output
  "q" 'gnus-summary-exit)

(evil-set-initial-state 'gnus-article-mode 'motion)

(provide 'init-evil-gnus)
