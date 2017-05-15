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
          (format "(%s@%s)[%s]\n%s "
                  (user-login-name)
                  (system-name)
                  path
                  (if (= (user-uid) 0) "#" ">")))))
;; If the prompt spans over multiple lines, the regexp should match last line only.
(setq eshell-prompt-regexp "^[#>] ")

(eval-after-load 'em-term
  '(nconc eshell-visual-commands
          '("abook" "cmus" "fzf" "htop" "mpv" "mutt" "ncdu" "newsbeuter" "ranger")))
; (eval-after-load 'em-term
;   '(add-to-list 'eshell-visual-subcommands '("git" "log" "diff" "show")))

;; eshell/alias is too slow as it reads and write the file on each definition.
(eval-after-load 'em-alias
  '(progn
     (eshell-read-aliases-list)
     (mapc
      (lambda (alias)
        (add-to-list 'eshell-command-aliases-list alias))
      '(("ls" "ls -F $*")
        ("l" "ls -1 $*")
        ("la" "ls -lAh $*")
        ("ll" "ls -lh $*")
        ("grep" "grep --color=auto")
        ("mkdir" "mkdir -p $*")
        ("mkcd" "mkdir -p $* && cd $1")
        ("emacs" "find-file $1")
        ("em" "find-file $1")))
     (eshell-write-aliases-list)))

(provide 'mode-eshell)
