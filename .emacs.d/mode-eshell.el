;; Eshell
(setq eshell-directory-name (concat emacs-cache-folder "eshell"))
;; (setq eshell-aliases-file (concat user-emacs-directory "eshell-alias"))
;; TODO: this breaks eshell completion and history.
;; (setq eshell-prompt-function
;;       (lambda nil
;;         (let ((path (abbreviate-file-name (eshell/pwd))))
;;           (concat ".-(" path ")"
;;                   (make-string (- (window-body-width) 5 (length path)) ?-)
;;                   "\n`--"
;;                   (if (= (user-uid) 0) "# " "> ")))))

(nconc eshell-visual-commands
       '("abook" "cmus" "htop" "mutt" "ncdu" "newsbeuter" "ranger"
         "rtorrent" "task" "tig"))
(when (file-executable-p "/usr/bin/pacman")
  (map-on-pair 'eshell/alias
               '(("pc" "sudo pacman -Sc")
                 ("pi" "sudo pacman -S --needed")
                 ("pqi" "pacman -Qi")
                 ("pqo" "pacman -Qo")
                 ("pqs" "pacman -Qs")
                 ("pr" "sudo pacman -Rs")
                 ("psi" "pacman -Si")
                 ("pss" "pacman -Ss")
                 ("pu" "sudo pacman -Syu"))))
(map-on-pair 'eshell/alias
             '(("mkdir" "mkdir -p")
               ("lx" "ls -lXh")
               ("lx" "ls -lXh")
               ("lk" "ls -lSrh")
               ("lc" "ls -lrc")
               ("ll" "ls -hl")
               ("la" "ls -ahl")))

(provide 'mode-eshell)
