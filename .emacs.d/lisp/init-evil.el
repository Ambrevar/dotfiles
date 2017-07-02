;; Evil

;; TODO: helm-show-yank-ring behaves like Emacs when pasting whole lines, not like Vim.

;; TODO: helm-mark-ring seems to have issues with Evil:
;; - The first entry is not the last position but the current one.
;; - Navigating through the marks randomly produces a "Marker points into wrong buffer" error.
;; https://github.com/emacs-evil/evil/issues/845#issuecomment-306050231

;; Several packages handle relative line numbering:
;; - nlinum-relative: Seems slow as of May 2017.
;; - linum-relative: integrates well but not with fringe string, must be a function.
;; - relative-line-number: linum must be disabled before running this.
(when (require 'linum-relative nil t)
  ;; REVIEW: Current symbol is displayed on all lines when we run `occur', `set-variables',
  ;; `helm-occur', etc: https://github.com/coldnew/linum-relative/issues/40.
  (setq linum-relative-current-symbol "")
  (linum-relative-toggle))

;; The evil-leader package has that over regular bindings that it centralizes
;; the leader key configuration and automatically makes it available in relevant
;; states.  Should we map <leader<leader> to the most used command,
;; e.g. `helm-mini'?  Could be misleading.
(require 'evil-leader)
;; Leader mode and its key must be set before evil-mode.
(evil-leader/set-leader "<SPC>")
(global-evil-leader-mode)

(evil-mode 1)
(remove-hook 'evil-insert-state-exit-hook 'expand-abbrev)
;; (setq evil-want-abbrev-expand-on-insert-exit nil)
(setq undo-tree-mode-lighter "")

;; Commenting.
;; M-; comments next line in VISUAL. This is because of a different newline
;; definition between Emacs and Vim.
;; https://github.com/redguardtoo/evil-nerd-commenter: does not work well with
;; motions and text objects, e.g. it cannot comment up without M--.
;; `evil-commentary' is the way to go. We don't need an additional minor-mode though.
(when (require 'evil-commentary nil t)
  (evil-global-set-key 'normal "gc" 'evil-commentary)
  (evil-global-set-key 'normal "gy" 'evil-commentary-yank))

(defun eshell-or-new-session (&optional arg)
  "Create an interactive Eshell buffer.
If there is already an Eshell session active, switch to it.
If current buffer is already an Eshell buffer, create a new one and switch to it.
See `eshell' for the numeric prefix arg."
  (interactive "P")
  (if (eq major-mode 'eshell-mode)
      (eshell (or arg t))
    (eshell arg)))

(defun org-find-first-agenda ()
  (interactive)
  (find-file (car org-agenda-files)))

(evil-leader/set-key
  "RET" 'eshell-or-new-session
  "\\" 'toggle-window-split
  "a" 'org-agenda
  "b" 'buffer-menu
  "e" 'find-file
  "k" 'kill-this-buffer
  "o" 'delete-other-windows
  "t" 'org-find-first-agenda
  "w" 'evil-window-next
  "|" 'swap-windows)
(when (fboundp 'magit-status)
  ;; Since it is an autoload, we cannot use `with-eval-after-load'.
  ;; Use S-SPC instead of SPC to browse commit details.
  (evil-leader/set-key "v" 'magit-status))
(when (fboundp 'emms-smart-browse)
  (evil-leader/set-key "M" 'helm-emms)
  (evil-leader/set-key "m" 'emms-smart-browse))
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

(with-eval-after-load 'init-helm
  ;; To navigate helm entries with hjkl, using the C- modifier would conflict
  ;; with C-h (help prefix) and C-k (`evil-insert-digraph').  We use M- instead.
  (define-keys helm-map
    "C-\\" 'helm-toggle-resplit-and-swap-windows
    "C-f" 'helm-next-page
    "C-b" 'helm-previous-page
    "M-h" 'helm-next-source
    "M-j" 'helm-next-line
    "M-k" 'helm-previous-line
    "M-l" 'helm-execute-persistent-action
    "M-." 'helm-end-of-buffer
    "M-," 'helm-beginning-of-buffer
    "<escape>" 'helm-keyboard-quit)
  (evil-define-key 'normal helm-map
    "\C-f" 'helm-next-page
    "\C-b" 'helm-previous-page)
  (define-key helm-buffer-map (kbd "M-o") 'helm-buffer-switch-other-window)
  (define-key helm-moccur-map (kbd "M-o") 'helm-moccur-run-goto-line-ow)
  (define-key helm-grep-map (kbd "M-o") 'helm-grep-run-other-window-action)
  (define-key helm-map (kbd "C-/") 'helm-quit-and-find-file)
  (dolist (keymap (list helm-find-files-map helm-read-file-map))
    (define-keys keymap
      "M-o" 'helm-ff-run-switch-other-window
      "C-/" 'helm-ff-run-find-sh-command
      "M-h" 'helm-find-files-up-one-level
      "M-l" 'helm-execute-persistent-action
      "C-l" nil))) ; So that the header displays the above binding.

;; Motion map: useful for `Info-mode', `help-mode', etc.
;; See `evil-motion-state-modes'.
(evil-global-set-key 'motion (kbd "TAB") 'forward-button)
(evil-global-set-key 'motion (kbd "<backtab>") 'backward-button)
(evil-define-key 'motion Info-mode-map
  (kbd "S-SPC") 'Info-scroll-up
  "\C-f" 'Info-scroll-up
  "\C-b" 'Info-scroll-down
  "\M-sf" 'Info-goto-node
  "gg" 'evil-goto-first-line)
(evil-define-key 'motion help-mode-map
  (kbd "S-SPC") 'scroll-up-command
  "\C-f" 'scroll-up-command
  "\C-b" 'scroll-down-command
  "\C-o" 'help-go-back)

;;; Term mode should be in emacs state. It confuses 'vi' otherwise.
;;; Upstream will not change this:
;;; https://github.com/emacs-evil/evil/issues/854#issuecomment-309085267
(evil-set-initial-state 'term-mode 'emacs)

;; Add support for magit.
(with-eval-after-load 'magit
  (when (require 'evil-magit nil t)
    (evil-magit-define-key evil-magit-state 'magit-mode-map "<" 'magit-section-up)
    ;; C-j/k is the default, M-j/k is more intuitive if we use it for helm.
    (evil-magit-define-key evil-magit-state 'magit-mode-map "M-j" 'magit-section-forward)
    (evil-magit-define-key evil-magit-state 'magit-mode-map "M-k" 'magit-section-backward)))

;; Add support for ediff.
(require 'evil-ediff nil t)

;; For git commit, web edits and others.
;; Since `with-editor-mode' is not a major mode, `evil-set-initial-state' cannot
;; be used.
(when (require 'with-editor nil t)
  (add-hook 'with-editor-mode-hook 'evil-insert-state))

;; Allow for evil states in minibuffer. Double <ESC> exits.
(dolist
    (keymap
     ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/
     ;; Text-from-Minibuffer.html#Definition of minibuffer-local-map
     '(minibuffer-local-map
       minibuffer-local-ns-map
       minibuffer-local-completion-map
       minibuffer-local-must-match-map
       minibuffer-local-isearch-map))
  (evil-define-key 'normal (eval keymap) [escape] 'abort-recursive-edit)
  (evil-define-key 'normal (eval keymap) [return] 'exit-minibuffer))

(defun evil-minibuffer-setup ()
  (set (make-local-variable 'evil-echo-state) nil)
  ;; (evil-set-initial-state 'mode 'insert) is the evil-proper
  ;; way to do this, but the minibuffer doesn't have a mode.
  ;; The alternative is to create a minibuffer mode (here), but
  ;; then it may conflict with other packages' if they do the same.
  (evil-insert 1))
(add-hook 'minibuffer-setup-hook 'evil-minibuffer-setup)
;; Because of the above minibuffer-setup-hook, some bindings need be reset.
(evil-define-key 'normal evil-ex-completion-map [escape] 'abort-recursive-edit)
(evil-define-key 'insert evil-ex-completion-map "\M-p" 'previous-complete-history-element)
(evil-define-key 'insert evil-ex-completion-map "\M-n" 'next-complete-history-element)
;; TODO: evil-ex history binding in normal mode do not work.
(evil-define-key 'normal evil-ex-completion-map "\M-p" 'previous-history-element)
(evil-define-key 'normal evil-ex-completion-map "\M-n" 'next-history-element)
(define-keys evil-ex-completion-map
  "M-p" 'previous-history-element
  "M-n" 'next-history-element)

;; Remap org-mode meta keys for convenience
;; - org-evil: Not as polished as of May 2017.
;; - evil-org: Depends on MELPA's org-mode, too big a dependency for me.
;; See https://github.com/Somelauw/evil-org-mode/blob/master/doc/keythemes.org for inspiration.
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

;;; Use evil-leader in Debugger until I take time to define keys properly.
(setq evil-leader/in-all-states t)
(setq evil-leader/no-prefix-mode-rx '("Debugger.*"))

;;; Package-menu mode
(evil-set-initial-state 'package-menu-mode 'normal)
(evil-define-key 'normal package-menu-mode-map "q" 'quit-window)
(evil-define-key 'normal package-menu-mode-map "i" 'package-menu-mark-install)
(evil-define-key 'normal package-menu-mode-map "U" 'package-menu-mark-upgrades)
(evil-define-key 'normal package-menu-mode-map "u" 'package-menu-mark-unmark)
(evil-define-key 'normal package-menu-mode-map "d" 'package-menu-mark-delete)
(evil-define-key 'normal package-menu-mode-map "x" 'package-menu-execute)

;; Eshell
(defun evil/eshell-next-prompt ()
  (when (get-text-property (point) 'read-only)
    ;; If at end of prompt, `eshell-next-prompt' will not move, so go backward.
    (beginning-of-line)
    (eshell-next-prompt 1)))
(defun evil/eshell-setup ()
  (dolist (hook '(evil-replace-state-entry-hook evil-insert-state-entry-hook))
    (add-hook hook 'evil/eshell-next-prompt nil t)))
(add-hook 'eshell-mode-hook 'evil/eshell-setup)

(defun evil/eshell-interrupt-process ()
  (interactive)
  (eshell-interrupt-process)
  (evil-insert 1))

;;; `eshell-mode-map' is reset when Eshell is initialized in `eshell-mode'. We
;;; need to add bindings to `eshell-first-time-mode-hook'.
(defun evil/eshell-set-keys ()
  (with-eval-after-load 'init-helm
    (evil-define-key 'insert eshell-mode-map "\C-e" 'helm-find-files))
  (evil-define-key 'normal eshell-mode-map "\M-k" 'eshell-previous-prompt)
  (evil-define-key 'normal eshell-mode-map "\M-j" 'eshell-next-prompt)
  (evil-define-key 'normal eshell-mode-map "0" 'eshell-bol)
  (evil-define-key 'normal eshell-mode-map (kbd "RET") 'eshell-send-input)
  (evil-define-key 'normal eshell-mode-map (kbd "C-c C-c") 'evil/eshell-interrupt-process)
  (evil-define-key '(normal insert) eshell-mode-map "\M-h" 'eshell-backward-argument)
  (evil-define-key '(normal insert) eshell-mode-map "\M-l" 'eshell-forward-argument))
(add-hook 'eshell-first-time-mode-hook 'evil/eshell-set-keys)

;; TODO: Make Evil commands react more dynamically with read-only text.
;; Add support for I, C, D, S, s, c*, d*, R, r.
;; See https://github.com/emacs-evil/evil/issues/852.

;; Go-to-definition.
;; From https://emacs.stackexchange.com/questions/608/evil-map-keybindings-the-vim-way.
(evil-global-set-key
 'normal "gd"
 (lambda () (interactive)
   (evil-execute-in-emacs-state)
   (call-interactively (key-binding (kbd "M-.")))))

;; Multiple cursors.
;; This shadows evil-magit's "gr", but we can use "?g" for that instead.
;; It shadows C-n/p (`evil-paste-pop'), but we use `helm-show-kill-ring' on
;; another binding.
(when (require 'evil-mc nil t)
  (global-evil-mc-mode 1)
  (define-key evil-mc-key-map (kbd "C-<mouse-1>") 'evil-mc-toggle-cursor-on-click)
  (set-face-attribute 'evil-mc-cursor-default-face nil :inherit nil :inverse-video nil :box "white")
  (when (require 'evil-mc-extras nil t)
    (global-evil-mc-extras-mode 1)))

;; nXML
(evil-define-key 'normal nxml-mode-map "<" 'nxml-backward-up-element)

;;; Emms
;;; It is important to set the bindings after emms-browser has loaded,
;;; since the mode-maps are defconst'd.
(with-eval-after-load 'emms-browser
  (dolist (mode '(emms-browser-mode emms-playlist-mode))
    (evil-set-initial-state mode 'normal))

  (defun evil/emms-playlist-mode-insert-newline-above ()
    "Insert a newline above point."
    (interactive)
    (emms-with-inhibit-read-only-t
     (evil-insert-newline-above)))

  (defun evil/emms-playlist-mode-insert-newline-below ()
    "Insert a newline below point."
    (interactive)
    (emms-with-inhibit-read-only-t
     (evil-insert-newline-below)))

  (defun evil/emms-playlist-mode-paste-before ()
    "Pastes the latest yanked playlist items before the cursor position.
The return value is the yanked text."
    (interactive)
    (emms-with-inhibit-read-only-t
     (goto-char (point-at-bol))
     (yank)
     (emms-playlist-mode-correct-previous-yank)
     (evil-previous-line)
     (evil-beginning-of-line)))

  (defun evil/emms-playlist-mode-paste-after ()
    "Pastes the latest yanked playlist items behind point.
The return value is the yanked text."
    (interactive)
    (evil-next-line)
    (evil/emms-playlist-mode-paste-before))

  (dolist (map (list emms-browser-mode-map emms-playlist-mode-map))
    (evil-define-key 'normal map
      "+" 'emms-volume-raise
      "=" 'emms-volume-raise
      "-" 'emms-volume-lower
      "u" 'emms-playlist-mode-undo))

  (evil-define-key 'normal emms-browser-mode-map
    (kbd "C-<return>") 'emms-browser-add-tracks-and-play
    (kbd "<return>") 'emms-browser-add-tracks
    (kbd "<tab>") 'emms-browser-toggle-subitems
    "/" 'emms-isearch-buffer ; This shows hidden items during search.
    "g1" 'emms-browser-collapse-all
    "g2" 'emms-browser-expand-to-level-2
    "g3" 'emms-browser-expand-to-level-3
    "g4" 'emms-browser-expand-to-level-4
    "<" 'emms-browser-previous-filter
    ">" 'emms-browser-next-filter
    "C" 'emms-browser-clear-playlist
    "D" 'emms-browser-delete-files
    "g0" 'emms-browser-expand-all
    "d" 'emms-browser-view-in-dired
    "\C-j" 'emms-browser-next-non-track
    "\C-k" 'emms-browser-prev-non-track
    "\M-j" 'emms-browser-next-non-track
    "\M-k" 'emms-browser-prev-non-track
    "[" 'emms-browser-prev-non-track
    "]" 'emms-browser-next-non-track
    "{" 'emms-browser-prev-non-track
    "}" 'emms-browser-next-non-track
    "ga" 'emms-browse-by-artist
    "gA" 'emms-browse-by-album
    "gb" 'emms-browse-by-genre
    "gy" 'emms-browse-by-year
    "gc" 'emms-browse-by-composer
    "gp" 'emms-browse-by-performer
    "x" 'emms-pause
    "s" (lookup-key emms-browser-mode-map (kbd "s"))
    "z" (lookup-key emms-browser-mode-map (kbd "W")))

  (evil-define-key 'normal emms-playlist-mode-map
    "o" 'evil/emms-playlist-mode-insert-newline-below
    "O" 'evil/emms-playlist-mode-insert-newline-above
    "d" 'emms-playlist-mode-kill-track
    (kbd "<return>") 'emms-playlist-mode-play-smart
    "P" 'evil/emms-playlist-mode-paste-before
    "p" 'evil/emms-playlist-mode-paste-after
    "u" 'emms-playlist-mode-undo
    "<" 'emms-seek-backward
    ">" 'emms-seek-forward
    "C" 'emms-playlist-mode-clear
    "D" 'emms-playlist-mode-kill-track
    "ze" 'emms-tag-editor-edit
    "x" 'emms-pause
    "R" 'emms-tag-editor-rename
    "a" 'emms-playlist-mode-add-contents
    "zp" 'emms-playlist-set-playlist-buffer
    "c" 'emms-playlist-mode-center-current
    "gd" 'emms-playlist-mode-goto-dired-at-point
    "zs" 'emms-show
    "\C-j" 'emms-next
    "\C-k" 'emms-previous
    "\M-j" 'emms-next
    "\M-k" 'emms-previous
    "r" 'emms-random
    "s" 'emms-stop
    "S" (lookup-key emms-playlist-mode-map (kbd "S"))
    "zf" (lookup-key emms-playlist-mode-map (kbd "/"))
    "zff" 'emms-playlist-limit-to-all
    "gg" 'emms-playlist-mode-first
    "G" 'emms-playlist-mode-last
    "]" 'emms-playlist-mode-next
    "[" 'emms-playlist-mode-previous
    "M-y" 'emms-playlist-mode-yank-pop)
  (evil-define-key 'visual emms-playlist-mode-map
    "d" 'emms-playlist-mode-kill
    "D" 'emms-playlist-mode-kill))

(provide 'init-evil)
