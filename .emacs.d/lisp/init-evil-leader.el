;;; Evil leader

;; Leader mode and its key must be set before evil-mode.
(evil-leader/set-leader "<SPC>")
(global-evil-leader-mode)

(evil-leader/set-key
 "RET" 'eshell-or-new-session
 "\\" 'toggle-window-split
 ;; "a" 'org-agenda
 "b" 'buffer-menu
 "e" 'find-file
 "k" 'kill-this-buffer
 "t" 'org-find-first-agenda
 "|" 'swap-windows)
(when (fboundp 'magit-status)
  ;; Since it is an autoload, we cannot use `with-eval-after-load'.
  ;; Use S-SPC instead of SPC to browse commit details.
  (evil-leader/set-key "v" 'magit-status))
(when (fboundp 'emms-smart-browse)
  (evil-leader/set-key "A" 'helm-emms)
  (evil-leader/set-key "a" 'emms-smart-browse))
(with-eval-after-load 'emms
  (evil-leader/set-key
   "p" 'emms-pause
   "n" 'emms-next))
(with-eval-after-load 'init-helm
  (evil-leader/set-key
   "b" 'helm-mini
   "e" 'helm-find-files
   "E" 'helm-find
   "g" 'helm-grep-git-or-ag
   "G" 'helm-grep-git-all-or-ag
   "r" 'helm-resume))

(when (fboundp 'mu4e)
  (require 'evil-mu4e nil t)
  (evil-leader/set-key "m" 'mu4e-headers-unread))

(provide 'init-evil-leader)
