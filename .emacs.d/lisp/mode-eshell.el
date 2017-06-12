;;; Eshell

;;; This mode has a lot of hooks.
;;; `emacs-load-hook' is run at the very beginning; not all variables/functions will be set.
;;; `emacs-mode-hook' is run once everything is loaded.

;;; TODO: Bind "ls"? No need if we have Ctrl-e?
;; (local-set-key "\C-l" 'eshell/ls)

(setq
 eshell-directory-name (concat emacs-cache-folder "eshell")
 eshell-banner-message ""
 eshell-ls-use-colors t
 eshell-destroy-buffer-when-process-dies t
 ;; TODO: Hour is printed twice. We don't need to set this?
 ;; eshell-ls-date-format (replace-regexp-in-string "^\\+*" "" (getenv "TIME_STYLE"))
 )

;;; Leave `eshell-highlight-prompt' to t as it sets the read-only property.
(setq eshell-prompt-function
      (lambda nil
        (let ((path (abbreviate-file-name (eshell/pwd))))
          (concat
           (format
            (propertize "(%s@%s)[%s]\n>" 'face '(:weight bold))
            (propertize (user-login-name) 'face '(:foreground "cyan"))
            (propertize (system-name) 'face '(:foreground "cyan"))
            (propertize path 'face `(:foreground ,(if (= (user-uid) 0) "red" "green") :weight bold)))
           " "))))
;;; If the prompt spans over multiple lines, the regexp should match
;;; last line only.
(setq eshell-prompt-regexp "^> ")

(with-eval-after-load 'em-term
  (nconc eshell-visual-commands
         '("abook" "cmus" "fzf" "htop" "mpv" "mutt" "ncdu" "newsbeuter" "ranger"))
  (add-to-list 'eshell-visual-subcommands '("git" "log" "diff" "show")))

;;; Alias management possibilities:
;;; - Version eshell-alias and store it in user-emacs-directory. Simplest and
;;; fastest, but aliases cannot be included conditionnaly, e.g. depending on the
;;; existence of a program.
;;; - Store eshell-alias in cache and populate it dynamically on startup.
;; (setq eshell-aliases-file (concat user-emacs-directory "eshell-alias"))
;;;
;;; `eshell/alias' is too slow as it reads and write the file on each definition.
;;; Let's write manually instead.
;;; TODO: Add pacman functions from fish config.
;;; TODO: Compare system tools and lisp equivalents of ls and grep.
(with-eval-after-load 'em-alias
  (eshell-read-aliases-list)
  (dolist (alias '(
                   ;; ("ls" "ls -F $*")
                   ("l" "ls -1 $*")
                   ("la" "ls -lAh $*")
                   ("ll" "ls -lh $*")
                   ;; ("grep" "grep --color=auto")
                   ("cal" "cal -m $*")
                   ("cp" "cp -i $*")
                   ("mv" "mv -i $*")
                   ("mkdir" "mkdir -p $*")
                   ("mkcd" "mkdir -p $* && cd $1")
                   ;; ("emacs" "find-file $1")
                   ("em" "find-file $1")))
    (add-to-list 'eshell-command-aliases-list alias))
  (eshell-write-aliases-list))

;;; Emacs' standard function fail when output has empty lines.
;;; This implementation is more reliable.
;;; TODO: Report upstream
(defun eshell-next-prompt (n)
  "Move to end of Nth next prompt in the buffer.
See `eshell-prompt-regexp'."
  (interactive "p")
  (re-search-forward eshell-prompt-regexp nil t n)
  (while (not (get-text-property (line-beginning-position) 'read-only) )
    (re-search-forward eshell-prompt-regexp nil t n))
  (eshell-skip-prompt))

(defun eshell-previous-prompt (n)
  "Move to end of Nth previous prompt in the buffer.
See `eshell-prompt-regexp'."
  (interactive "p")
  (backward-char)
  (eshell-next-prompt (- n)))

(provide 'mode-eshell)
