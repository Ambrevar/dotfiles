;; Evil

;; TODO: Turn off bindings from tool-helm.el when evil is loaded?
;; TODO: Add hjkl to debugger output mode.
;; TODO: Navigate visual lines with j/k?
;; TODO: M-; comments next line in VISUAL. This is because of a different newline definition between Emacs and Vim.
;; https://github.com/redguardtoo/evil-nerd-commenter
;; Not so different, cannot comment up without M--.

;; Several packages handle relative line numbering:
;; - nlinum-relative: Seems slow as of May 2017.
;; - linum-relative: integrates well but not with fringe string, must be a function.
;; - relative-line-number: linum must be disabled before running this.
(when (require 'linum-relative nil t)
  ;; TODO: Current symbol is displayed on all lines in occur, set-variables,
  ;; helm-occur, etc. https://github.com/coldnew/linum-relative/issues/40
  (setq linum-relative-current-symbol "")
  (linum-relative-toggle))

;; The evil-leader package might seem worthless but it centralizes the leader
;; key configuration and automatically makes it available in relevant states. We
;; do not map <leader><leader> to most used command since it could be
;; misleading. Maybe the helm buffer menu would be interesting to have though.
(when (require 'evil-leader nil t)
  ;; TODO: dired/info can not super <SPC> as leader. Use ',' as leader key?
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "RET" 'spawn-terminal
    "\\" 'toggle-window-split
    "b" 'buffer-menu
    "e" 'find-file
    "h" 'help-command ; Does not work in insert mode. M-h creates conflicts, so let's keep C-h for insert mode.
    "k" 'kill-this-buffer ; TODO: In Vim its :bd, so should we map to " d"? Then git project should be something else.
    "o" 'delete-other-windows
    "w" 'evil-window-next
    "|" 'swap-windows ; TODO: Map to " x" (Vim has ^W x)? helm-marks would then need to be mapped to '' for instance.
    )
  (when (require 'magit nil t)
    ;; Use S-SPC instead of SPC to browse commit details.
    (evil-leader/set-key "v" 'magit-status))
  (when (require 'helm-config nil t)
    (evil-leader/set-key
      "r" 'helm-resume
      "b" 'helm-mini
      "d" 'helm-browse-project
      "e" 'helm-find-files
      "E" 'helm-find
      "g" 'helm-grep-git-or-ag
      "G" 'helm-grep-git-all-or-ag
      "ha" 'helm-apropos))
  ;; leader mode must be set before evil-mode.
  (global-evil-leader-mode))

(evil-mode 1)

(evil-global-set-key 'normal "\M-6" 'evil-switch-to-windows-last-buffer)
(evil-global-set-key 'normal (kbd "M-v") 'evil-visual-block)
(evil-global-set-key 'motion (kbd "M-b") 'evil-scroll-page-up)
(evil-global-set-key 'motion (kbd "M-f") 'evil-scroll-page-down)
(evil-global-set-key 'motion (kbd "M-r") 'undo-tree-redo)

;; Tweak motion map: useful for info-mode, help-mode, etc.
;; See `evil-motion-state-modes'.
;; TODO: Map n/p, l/r for help-mode and Info-mode.
(evil-global-set-key 'motion (kbd "TAB") 'forward-button)
(evil-global-set-key 'motion (kbd "<backtab>") 'backward-button)
(evil-define-key 'motion Info-mode-map "\M-f" 'Info-scroll-up)
(evil-define-key 'motion Info-mode-map "\M-b" 'Info-scroll-down)

(when (require 'helm-config nil t)
  ;; This separates Vim marks into Emacs marks and Emacs bookmarks.
  (evil-global-set-key 'normal "m" 'helm-filtered-bookmarks)
  ;; TODO: This does not work well together with Evil.
  ;; Alternative: show-marks with helm?
  ;; Back to where search started with C-o. Alternative without Ctrl? Seems that '' does it.
  (evil-global-set-key 'normal "'" 'helm-mark-or-exchange-rect)
  ;; TODO: Yank ring pasting behaves like Emacs, not Vim.
  (evil-global-set-key 'normal "\M-p" 'helm-show-kill-ring)

  (define-key helm-map (kbd "M-\\") 'helm-toggle-resplit-and-swap-windows) ; Or use M-t (helm standard binding is C-t).
  (define-key helm-map (kbd "M-f") 'helm-next-page)
  (define-key helm-map (kbd "M-b") 'helm-previous-page)
  (define-key helm-map (kbd "M-j") 'helm-next-line)
  (define-key helm-map (kbd "M-k") 'helm-previous-line)
  (define-key helm-map (kbd "M-h") 'helm-next-source)
  (define-key helm-map (kbd "M-H") 'describe-key)
  (define-key helm-map (kbd "M-l") (kbd "RET"))
  (define-key helm-map [escape] 'helm-keyboard-quit)
  (dolist (keymap (list helm-buffer-map))
    (define-key keymap (kbd "M-o") 'helm-buffer-switch-other-window))
  (dolist (keymap (list helm-find-files-map helm-read-file-map))
    (define-key keymap (kbd "M-o") 'helm-ff-run-switch-other-window)
    (define-key keymap (kbd "M-.") 'helm-ff-run-find-sh-command)
    (define-key keymap (kbd "M-l") 'helm-execute-persistent-action)
    (define-key keymap (kbd "M-h") 'helm-find-files-up-one-level)
    (define-key keymap (kbd "M-H") 'describe-key)))

;; TODO: Define ex commands? Useful for short calls with arguments.
;; (evil-ex-define-cmd "buffers" 'helm-buffers-list)
;; (evil-ex-define-cmd "ls" 'helm-buffers-list)
;; (evil-ex-define-cmd "edit" 'helm-find-files)

(when (and (require 'magit-mode nil t) (require 'evil-magit nil t))
  (evil-magit-define-key evil-magit-state 'magit-mode-map "M-j" 'magit-section-forward)
  (evil-magit-define-key evil-magit-state 'magit-mode-map "M-k" 'magit-section-backward))

;; For git commit, web edits and others.
;; Since `with-editor-mode' is not a major mode, `evil-set-initial-state' cannot
;; be used.
(when (require 'with-editor nil t)
  (add-hook 'with-editor-mode-hook 'evil-insert-state))

;; Allow for evil states in minibuffer. Double <ESC> exits.
;; TODO: Double <ESC> does not exit EX prompt.
;; (evil-define-key 'normal 'evil-ex-map [escape] 'abort-recursive-edit)
(dolist
    (keymap
     ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/
     ;; Text-from-Minibuffer.html#Definition of minibuffer-local-map
     '(minibuffer-local-map
       minibuffer-local-ns-map
       minibuffer-local-completion-map
       minibuffer-local-must-match-map
       minibuffer-local-isearch-map))
  ;; (evil-define-key 'insert (eval keymap) [escape] 'abort-recursive-edit)
  ;; (evil-define-key 'insert (eval keymap) [return] 'exit-minibuffer)
  ;; (evil-define-key 'insert (eval keymap) "\M-z" 'evil-normal-state))
  (evil-define-key 'normal (eval keymap) [escape] 'abort-recursive-edit)
  (evil-define-key 'normal (eval keymap) [return] 'exit-minibuffer))

(add-hook
 'minibuffer-setup-hook
 '(lambda ()
    (set (make-local-variable 'evil-echo-state) nil)
    ;; (evil-set-initial-state 'mode 'insert) is the evil-proper
    ;; way to do this, but the minibuffer doesn't have a mode.
    ;; The alternative is to create a minibuffer mode (here), but
    ;; then it may conflict with other packages' if they do the same.
    (evil-insert 1)))

;; Remap org-mode meta keys for convenience
;; - org-evil: Not as polished as of May 2017.
;; - evil-org: Depends on MELPA's org-mode, too big a dependency for me.
;; See https://github.com/Somelauw/evil-org-mode/blob/master/doc/keythemes.org for inspiration.
(add-hook
 'org-mode-hook
 (lambda ()
   (when (require 'evil-leader nil t)
     (evil-leader/set-key-for-mode 'org-mode "a" 'org-agenda))
   (dolist (state '(normal insert))
     (evil-define-key state org-mode-map
       (kbd "<") 'org-up-element
       (kbd "M-l") 'org-shiftright
       (kbd "M-h") 'org-shiftleft
       (kbd "M-k") 'org-shiftup
       (kbd "M-j") 'org-shiftdown
       (kbd "M-L") 'org-shiftmetaright
       (kbd "M-H") 'org-shiftmetaleft
       (kbd "M-K") 'org-shiftmetaup
       (kbd "M-J") 'org-shiftmetadown))))

;; Package-menu mode
(delete 'package-menu-mode evil-emacs-state-modes)
(evil-define-key 'normal package-menu-mode-map "i" 'package-menu-mark-install)
(evil-define-key 'normal package-menu-mode-map "U" 'package-menu-mark-upgrades)
(evil-define-key 'normal package-menu-mode-map "u" 'package-menu-mark-unmark)
(evil-define-key 'normal package-menu-mode-map "d" 'package-menu-mark-delete)
(evil-define-key 'normal package-menu-mode-map "x" 'package-menu-execute)

;; Go-to-definition.
;; From https://emacs.stackexchange.com/questions/608/evil-map-keybindings-the-vim-way
(evil-global-set-key
 'normal "gd"
 (lambda () (interactive)
   (evil-execute-in-emacs-state)
   (call-interactively (key-binding (kbd "M-.")))))

;; Multiple cursors.
;; TODO: Not able to execute arbitrary commands with multiple cursors.
;; https://github.com/gabesoft/evil-mc/issues/65
(when (require 'evil-mc nil t)
  (global-evil-mc-mode 1)
  (when (require 'evil-mc-extras nil t)
    (global-evil-mc-extras-mode 1)))

(provide 'tool-evil)
