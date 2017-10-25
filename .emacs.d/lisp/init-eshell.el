;;; Eshell

;;; Eshell gets initialized differently.  When eshell.el first gets loaded, only
;;; the core is defined and `eshell-load-hook' is called. For every Eshell
;;; session, `eshell-mode' is run: it resets `eshell-mode-map', it loads
;;; modules, runs their hooks and concludes with `eshell-first-time-mode-hook'
;;; (for the first session only) and `eshell-mode-hook'.

;; Emacs pinentry for GPG.
(require 'main)

;;; REVIEW: ANSI coloring goes wrong sometimes.  Quite often with emerge/eix.
;;; Fixed in #27407.
(require 'patch-eshell)

;;; TODO: Sometimes transmission-daemon does not start from Eshell.

;;; TODO: Redirecting big output to file (e.g. /dev/null) is extremely slow.
;; > cat /usr/share/dict/british-english | wc -l
;;; The above line yields rancom results.  Plus it's much slower than
;; > wc -l /usr/share/dict/british-english
;;; while it should only cost an additional process to launch.

;;; REVIEW: Cannot "C-c C-c" during a `sudo pacman -Syu`.  A bug was filed about that already.

;;; TODO: The buffer stutters when writing "in-place", e.g. pacman, git.
;;; It seems that it does not do it as much in `emacs -Q`.

;;; REVIEW: `eshell/sudo' should not read -u in command arguments.
;;; This fails: sudo pacman -Syu --noconfirm.
;;; http://www.gnu.org/software/emacs/manual/html_node/eshell/Built_002dins.html
;;; https://emacs.stackexchange.com/questions/5608/how-to-let-eshell-remember-sudo-password-for-two-minutes
;;; See http://debbugs.gnu.org/cgi/bugreport.cgi?bug=27411
;;; and #28323.

;;; REVIEW: Eshell/Shell completion fails when PATH has a non-readable element.
;;; See https://github.com/emacs-helm/helm/issues/1785
;;; and https://debbugs.gnu.org/cgi/bugreport.cgi?bug=27300.

;;; REVIEW: 40M+ output: Stack overflow in regexp matcher
;;; See bug#28329.

;;; REVIEW: Eshell mixes stderr and stdout it seems.
;;; Example:
;;; $ mu find --nocolor --sortfield=d --maxnum=500 flag:unread AND NOT flag:trashed >/dev/null
;;; mu: no matches for search expression (4)
;;; See #21605 "24.3; Eshell not using stderr".

;;; TODO: Some parsing fails
;;; > echo -n $PATH | sed 's/:[^:]*sophos[^:]*/:/g'
;;; :s/:]*sophos[/:]*/:/"/
;;; Unknown modifier character ‘/’
;;;
;;; > date +%Z

;;; TODO: Hour is printed twice. We don't need to set this?
;; (setq eshell-ls-date-format (replace-regexp-in-string "^\\+*" "" (getenv "TIME_STYLE")))

;;; TODO: ls: Sort using locale.

;;; TODO: `kill -#' does not work.

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
  (dolist (p '("abook" "alsamixer" "cmus" "dtach" "fzf" "htop" "mpsyt" "mpv" "mutt" "ncdu" "newsbeuter" "pinentry-curses" "ranger" "watch" "wifi-menu"))
    (add-to-list 'eshell-visual-commands p))
  (setq eshell-visual-subcommands
        '(("git" "log" "diff" "show"
           "l" "lol" "d" "dc") ; aliases
          ("sudo" "wifi-menu") ; Arch Linux
          ("sudo" "vi" "visudo"))))

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
  ;;;
  ;;; If we read the alias list here, it means we make commandline-defined aliases persistent.
  ;; (eshell-read-aliases-list)
  (dolist
      (alias
       '(("l" "ls -1 $*")
         ("la" "ls -lAh $*")
         ("ll" "ls -lh $*")
         ;; TODO: Aliasing eshell/{cp,mv,ln} does not work.
         ;; TODO: "sudo" does not work on aliases.
         ;; See bug #27168.
         ;; REVIEW: Eshell/TRAMP's sudo does not work with aliases.
         ;; See #28320.
         ;; ("ls" "ls -F $*") ; not supported
         ;; ("emacs" "find-file $1")
         ;; ("cp" "eshell/cp -iv $*")
         ;; ("mv" "eshell/mv -iv $*")
         ("cpv" "cp -iv $*")
         ("mvv" "mv -iv $*")
         ("rmv" "rm -v $*")
         ("md" "eshell/mkdir -p $*")
         ("mkcd" "eshell/mkdir -p $* && cd $1"))) ; TODO: Does not work because mkdir exits with nil?
    (add-to-list 'eshell-command-aliases-list alias))
  (eshell-write-aliases-list))

;;; Hooks
;;; `nobreak-char-display' makes some output look weird, e.g. with 'tree'.
(add-hook 'eshell-mode-hook 'turn-off-nobreak-char-display)
(add-hook 'eshell-mode-hook 'eshell-cmpl-initialize)

;;; History
;;; Filter out space-beginning commands from history.
;;; TODO: history/command hook: trim surrounding space.  Check `eshell-rewrite-command-hook'.
;;; TODO: history: do not save failed commands to file.
;;; TODO: history: do not store duplicates.  Push unique command to front of the list.
(setq eshell-input-filter
      (lambda (str)
        (not (or (string= "" str)
                 (string-prefix-p " " str)))))

;;; Shared history.
(defvar eshell-history-global-ring nil
  "The history ring shared across Eshell sessions.")

(defun eshell-hist-use-global-history ()
  "Make Eshell history shared across different sessions."
  (unless eshell-history-global-ring
    (let (eshell-history-ring)
      (when eshell-history-file-name
        (eshell-read-history nil t))
      (setq eshell-history-global-ring eshell-history-ring))
    (unless eshell-history-ring (setq eshell-history-global-ring (make-ring eshell-history-size))))
  (setq eshell-history-ring eshell-history-global-ring))
(add-hook 'eshell-mode-hook 'eshell-hist-use-global-history)

;;; Spawning
(defun eshell-or-new-session (&optional arg)
  "Create an interactive Eshell buffer.
Switch to last Eshell session if any.
Otherwise create a new one and switch to it.
See `eshell' for the numeric prefix ARG."
  (interactive "P")
  (if (or arg (eq major-mode 'eshell-mode))
      (eshell (or arg t))
    (let ((last (buffer-list)))
      (while (and last
                  (not (with-current-buffer (car last)
                         (eq major-mode 'eshell-mode))))
        (setq last (cdr last)))
      (if last
          (switch-to-buffer (car last))
        (eshell (or arg t))))))

(when (require 'bash-completion nil t)
  ;; Sometimes `eshell-default-completion-function' does better, e.g. "gcc
  ;; <TAB>" shows .c files.
  (setq eshell-default-completion-function 'eshell-bash-completion))

(defun eshell-bash-completion ()
  (while (pcomplete-here
          (nth 2 (bash-completion-dynamic-complete-nocomint (save-excursion (eshell-bol) (point)) (point))))))

;;; TODO: Publish fish completion.
;;; If the user fish config change directory on startup, file completion will
;;; not be right.  One work-around is to add a "cd default-directory" before the
;;; "complete", but that's brittle because of unreliable shell escaping.
;;; Upstream does not allow for skipping the user config:
;;; https://github.com/fish-shell/fish-shell/issues/4165.
(defvar fish-completion-command "fish"
  "The `fish' executable.")

;; TODO: Make minor mode for buffer-local completion?  Probably not worth it.
(defun fish-completion-eshell-global-toggle ()
  "Turn on/off fish shell completion in all future Eshells.
Eshell fallbacks on fish whenever it cannot complete normally."
  (interactive)
  (if (or (eq eshell-default-completion-function 'fish-completion-eshell-complete)
          (not (executable-find fish-completion-command)))
      (progn
        (setq eshell-default-completion-function (eval (car (get 'eshell-default-completion-function 'standard-value))))
        (message "fish completion disabled in all future Eshells"))
    (setq eshell-default-completion-function 'fish-completion-eshell-complete)
    (message "fish completion enabled in all future Eshells")))

(defun fish-completion-eshell-toggle ()
  "Turn on/off fish shell completion in current Eshell.
Eshell fallbacks on fish whenever it cannot complete normally."
  (interactive)
  (if (or (eq pcomplete-default-completion-function 'fish-completion-eshell-complete)
          (not (executable-find fish-completion-command)))
      (progn
        (setq pcomplete-default-completion-function (eval (car (get 'eshell-default-completion-function 'standard-value))))
        (message "fish completion disabled in current Eshell"))
    (setq pcomplete-default-completion-function 'fish-completion-eshell-complete)
    (message "fish completion enabled in current Eshell")))

(defun fish-completion-eshell-complete ()
  "Complete Eshell's prompt with `fish-completion-complete'."
  (fish-completion-complete (buffer-substring-no-properties
                             (save-excursion (eshell-bol) (point)) (point))))

(defun fish-completion-complete (raw-prompt)
  "Complete RAW-PROMPT (any string) using the fish shell."
  (while (pcomplete-here
          (let ((comp-list
                 (let* (;; Keep spaces at the end with OMIT-NULLS=nil in `split-string'.
                        (toks (split-string raw-prompt split-string-default-separators nil))
                        ;; The first non-empty `car' is the command.  Discard
                        ;; leading empty strings.
                        (tokens (progn (while (string= (car toks) "")
                                         (setq toks (cdr toks)))
                                       toks))
                        ;; Fish does not support subcommand completion.  We make
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
                   ;; Completion result can be a filename.  pcomplete expects
                   ;; cannonical file names (i.e. without '~') while fish preserves
                   ;; non-cannonical results.  If the result contains a directory,
                   ;; expand it.
                   (mapcar (lambda (e) (car (split-string e "\t")))
                           (split-string
                            (with-output-to-string
                              (with-current-buffer standard-output
                                (call-process fish-completion-command nil t nil "-c" (format "complete -C'%s'" prompt))))
                            "\n" t)))))
            (if (and comp-list (file-name-directory (car comp-list)))
                (pcomplete-dirs-or-entries)
              comp-list)))))

(provide 'init-eshell)
