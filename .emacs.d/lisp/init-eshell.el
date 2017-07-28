;;; Eshell

;;; Eshell gets initialized differently.  When eshell.el first gets loaded, only
;;; the core is defined and `eshell-load-hook' is called. For every Eshell
;;; session, `eshell-mode' is run: it resets `eshell-mode-map', it loads
;;; modules, runs their hooks and concludes with `eshell-first-time-mode-hook'
;;; (for the first session only) and `eshell-mode-hook'.

;;; REVIEW: Eshell/Shell completion fails when PATH has a non-readable element.
;; See https://github.com/emacs-helm/helm/issues/1785
;; and https://debbugs.gnu.org/cgi/bugreport.cgi?bug=27300.

(setq eshell-directory-name (concat emacs-cache-folder "eshell"))

;;; Use native 'sudo', system sudo asks for password every time.
(require 'em-tramp)

(with-eval-after-load "esh-module" ; Need a file name because `provide' is before the definition of `eshell-modules-list. TODO: Report.
  ;; Don't print the banner.
  (delq 'eshell-banner eshell-modules-list)
  (push 'eshell-tramp eshell-modules-list))

(setq
 eshell-ls-use-colors t
 ;; ffap-shell-prompt-regexp changes the behaviour of `helm-find-files' when
 ;; point is on prompt. I find this disturbing.
 ffap-shell-prompt-regexp nil
 eshell-history-size 1024
 eshell-hist-ignoredups t
 eshell-destroy-buffer-when-process-dies t)

;; TODO: Hour is printed twice. We don't need to set this?
;; (setq eshell-ls-date-format (replace-regexp-in-string "^\\+*" "" (getenv "TIME_STYLE")))

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
(setq-default eshell-prompt-regexp "^> ")

(with-eval-after-load 'em-term
  (nconc eshell-visual-commands
         '("abook" "alsamixer" "cmus" "fzf" "htop" "mpsyt" "mpv" "mutt" "ncdu" "newsbeuter" "pinentry-curses" "ranger" "watch" "wifi-menu"))
  (setq eshell-visual-subcommands
        '(("git" "log" "l" "lol" "diff" "d" "dc" "show")
          ("sudo" "wifi-menu")
          ("sudo" "vi"))))

;;; Support for Emacs' pinentry
;; (setq epa-pinentry-mode 'loopback) ; This will fail if gpg>=2.1 is not available.
(pinentry-start)

;;; Alias management possibilities:
;;; - Version eshell-alias and store it in user-emacs-directory. Simplest and
;;; fastest, but aliases cannot be included conditionnaly, e.g. depending on the
;;; existence of a program.
;;; - Store eshell-alias in cache and populate it dynamically on startup.
;; (setq eshell-aliases-file (concat user-emacs-directory "eshell-alias"))
;;;
;;; `eshell/alias' is too slow as it reads and write the file on each definition.
;;; Let's write manually instead.
(with-eval-after-load 'em-alias
  ;;; TODO: This conflicts with `evil-define-key' during the initialization of
  ;;; the first eshell session: the map in insert-mode will not take the changes
  ;;; into account. Going to normal mode and back to insert mode works.
  (eshell-read-aliases-list)
  (dolist
      (alias
       '(("l" "ls -1 $*")
         ("la" "ls -lAh $*")
         ("ll" "ls -lh $*")
         ;; ("ls" "ls -F $*")
         ;; ("emacs" "find-file $1")
         ;; ("em" "find-file $*")
         ("cp" "*cp -i $*")
         ("mv" "*mv -i $*")
         ("mkdir" "*mkdir -p $*")
         ("mkcd" "*mkdir -p $* && cd $1")))
    (add-to-list 'eshell-command-aliases-list alias))
  (eshell-write-aliases-list))

;;; Hooks
;;; `nobreak-char-display' makes some output look weird, e.g. with 'tree'.
(add-hook 'eshell-mode-hook 'turn-off-nobreak-char-display)
(add-hook 'eshell-mode-hook 'eshell-cmpl-initialize)

;;; Filter out space-beginning commands from history.
(setq eshell-input-filter
      (lambda (str)
        (not (or (string= "" str)
                 (string-prefix-p " " str)))))

(defun eshell-or-new-session (&optional arg)
  "Create an interactive Eshell buffer.
If there is already an Eshell session active, switch to it.
If current buffer is already an Eshell buffer, create a new one and switch to it.
See `eshell' for the numeric prefix ARG."
  (interactive "P")
  (if (eq major-mode 'eshell-mode)
      (eshell (or arg t))
    (eshell arg)))

;;; REVIEW: Emacs' standard functions fail when output has empty lines.
;;; This implementation is more reliable.
;;; Reported at https://debbugs.gnu.org/cgi/bugreport.cgi?bug=27405.
(with-eval-after-load 'em-prompt
  (defun eshell-next-prompt (n)
    "Move to end of Nth next prompt in the buffer.
See `eshell-prompt-regexp'."
    (interactive "p")
    (re-search-forward eshell-prompt-regexp nil t n)
    (when eshell-highlight-prompt
      (while (not (get-text-property (line-beginning-position) 'read-only) )
        (re-search-forward eshell-prompt-regexp nil t n)))
    (eshell-skip-prompt))

  (defun eshell-previous-prompt (n)
    "Move to end of Nth previous prompt in the buffer.
See `eshell-prompt-regexp'."
    (interactive "p")
    (backward-char)
    (eshell-next-prompt (- n))))

(when (require 'bash-completion nil t)
  ;; REVIEW: `bash-completion-dynamic-complete' is almost what we want for eshell, it only needs a tiny modification.
  ;; Ask upstream to change the function arguments.
  ;; See https://github.com/szermatt/emacs-bash-completion/issues/13.
  ;;
  ;; Sometimes `eshell-default-completion-function' does better, e.g. "gcc
  ;; <TAB>" shows .c files.
  (setq eshell-default-completion-function
        (lambda ()
          (while (pcomplete-here
                  (cl-letf (((symbol-function 'comint-line-beginning-position)
                             #'(lambda () (save-excursion (eshell-bol) (point)))))
                    (nth 2 (bash-completion-dynamic-complete))))))))

;;; If the user fish config change directory on startup, file completion will
;;; not be right.  One work-around is to add a "cd default-directory" before the
;;; "complete", but that's brittle because of unreliable shell escaping.
;;; Upstream does not allow for skipping the user config:
;;; https://github.com/fish-shell/fish-shell/issues/4165.
(when (executable-find "fish")
  (setq eshell-default-completion-function
        (lambda ()
          (while (pcomplete-here
                  (let* ((raw-prompt (buffer-substring-no-properties (save-excursion (eshell-bol) (point)) (point)))
                         ;; Keep spaces at the end with OMIT-NULLS=nil in `split-string'.
                         (toks (split-string raw-prompt split-string-default-separators nil))
                         ;; We want the command (i.e. the first 'car') to be the
                         ;; command.  Discard potential empty strings.
                         (tokens (progn (while (string= (car toks) "")
                                          (setq toks (cdr toks)))
                                        toks))
                         ;; Fish does not support subcommand completion. We make
                         ;; a special case of 'sudo' and 'env' since they are
                         ;; the most common cases involving subcommands.  See
                         ;; https://github.com/fish-shell/fish-shell/issues/4093.
                         (prompt (if (not (member (car tokens) '("sudo" "env")))
                                     raw-prompt
                                   (setq tokens (cdr tokens))
                                   (while (and tokens
                                               (or (string-match "^-.*" (car tokens))
                                                   (string-match "=" (car tokens))))
                                     ;; Skip env/sudo parameters, like LC_ALL=C.
                                     (setq tokens (cdr tokens)))
                                   (mapconcat 'identity tokens " "))))
                    (mapcar (lambda (e) (car (split-string e "\t")))
                            (split-string
                             (with-output-to-string
                               (with-current-buffer standard-output
                                 (call-process "fish" nil t nil "-c" (format "complete -C'%s'" prompt))))
                             "\n" t))))))))

(provide 'init-eshell)
