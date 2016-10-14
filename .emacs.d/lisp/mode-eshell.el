;; Eshell

;; This mode has a lot of hooks.
;; `emacs-load-hook' is run at the very beginning; not all variables/functions will be set.
;; `emacs-mode-hook' is run once everything is loaded.

(setq eshell-directory-name (concat emacs-cache-folder "eshell"))

;; Alias management possibilities:
;; - Version eshell-alias and store it in user-emacs-directory. Simplest and
;; fastest, but aliases cannot be included conditionnaly, e.g. depending on the
;; existence of a program.
;; - Store eshell-alias in cache and populate it dynamically on startup.
; (setq eshell-aliases-file (concat user-emacs-directory "eshell-alias"))
(setq eshell-prompt-function
      (lambda nil
        (let ((path (abbreviate-file-name (eshell/pwd))))
          (concat ".-(" path ")"
                  (make-string (- (window-body-width) 5 (length path)) ?-)
                  "\n`--"
                  (if (= (user-uid) 0) "# " "> ")))))
;; If the prompt spans over multiple lines, the regexp should match last line only.
(setq eshell-prompt-regexp "^`--[#>] ")

(eval-after-load 'em-term
  '(nconc eshell-visual-commands
          '("abook" "cmus" "fzf" "htop" "mpv" "mutt" "ncdu" "newsbeuter" "ranger")))
; (eval-after-load 'em-term
;   '(add-to-list 'eshell-visual-subcommands '("git" "log" "diff" "show")))

;; eshell/alias is too slow as it reads and write the file on each definition.
(eval-after-load 'em-alias
  '(progn
     (eshell-read-aliases-list)
     (mapcar
      (lambda (alias)
        (add-to-list 'eshell-command-aliases-list alias))
      '(("ls" "ls -F $*")
        ("l" "ls -1 $*")
        ("la" "ls -lAh $*")
        ("lc" "ls -ltcrh $*")
        ("lk" "ls -lSrh $*")
        ("ll" "ls -lh $*")
        ("lx" "ls -lXh $*")
        ("grep" "grep --color=auto")
        ("mkdir" "mkdir -p $*")
        ("mkcd" "mkdir -p $* && cd $1")
        ("emacs" "find-file $1")
        ("em" "find-file $1")))
     (when (executable-find "/usr/bin/pacman")
       (mapcar
        (lambda (alias)
          (add-to-list 'eshell-command-aliases-list alias))
        '(("pc" "sudo pacman -Sc")
          ("pi" "sudo pacman -S --needed $*")
          ("pqi" "pacman -Qi $*")
          ("pqo" "pacman -Qo $*")
          ("pqs" "pacman -Qs $*")
          ("pr" "sudo pacman -Rs $*")
          ("psi" "pacman -Si $*")
          ("pss" "pacman -Ss $*")
          ("pu" "yes | sudo pacman -Sc && sudo pacman -Syu")
          ("pql" "pacman -Ql $* | /usr/bin/grep -v '/$'")
          ("pli" "expac -t '%F %T' '%-8l %n' | sort -rn | head -30")
          ("pacfiles" "pacman -Qlq ncdu | /usr/bin/grep -v '/$' | xargs du -cbh | sort -h"))))
     (eshell-write-aliases-list)))

(provide 'mode-eshell)
