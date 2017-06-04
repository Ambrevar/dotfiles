;; Evil

;; TODO: Yank ring pasting behaves like Emacs, not Vim.
;; TODO: show-marks with helm? Evil mixes up the mark ring. Back to where search started with C-o or ''(?).
;; TODO: Evil seems marks in the wrong ring sometimes.

;; TODO: M-; comments next line in VISUAL. This is because of a different
;; newline definition between Emacs and Vim.
;; https://github.com/redguardtoo/evil-nerd-commenter: Not so different, cannot
;; comment up without M--.

;; Several packages handle relative line numbering:
;; - nlinum-relative: Seems slow as of May 2017.
;; - linum-relative: integrates well but not with fringe string, must be a function.
;; - relative-line-number: linum must be disabled before running this.
(when (require 'linum-relative nil t)
  ;; TODO: Current symbol is displayed on all lines when we run `occur', `set-variables',
  ;; `helm-occur', etc: https://github.com/coldnew/linum-relative/issues/40
  (setq linum-relative-current-symbol "")
  (linum-relative-toggle))

;; The evil-leader package might seem worthless but it centralizes the leader
;; key configuration and automatically makes it available in relevant states.
;; We do not map <leader><leader> to most used command since it could be
;; misleading. Maybe the helm buffer menu would be interesting to have though?
(when (require 'evil-leader nil t)
  ;; Leader mode and its key must be set before evil-mode.
  (evil-leader/set-leader "<SPC>")
  (global-evil-leader-mode))

(evil-mode 1)

(evil-leader/set-key
  "RET" 'spawn-terminal
  "\\" 'toggle-window-split
  "b" 'buffer-menu
  "e" 'find-file
  "k" 'kill-this-buffer
  "o" 'delete-other-windows
  "w" 'evil-window-next
  "|" 'swap-windows
  )
(when (require 'magit nil t)
  ;; Use S-SPC instead of SPC to browse commit details.
  (evil-leader/set-key "v" 'magit-status))

;; Tweak motion map: useful for info-mode, help-mode, etc.
;; See `evil-motion-state-modes'.
(evil-global-set-key 'motion (kbd "TAB") 'forward-button)
(evil-global-set-key 'motion (kbd "<backtab>") 'backward-button)
(evil-define-key 'motion Info-mode-map (kbd "S-SPC") 'Info-scroll-up)
(evil-define-key 'motion help-mode-map (kbd "S-SPC") 'scroll-up-command)
(evil-define-key 'motion help-mode-map (kbd "C-o") 'help-go-back)

;; This depends on the local configuration of Helm which might not be loaded
;; yet.
(with-eval-after-load 'tool-helm
  (evil-leader/set-key
    "b" 'helm-mini
    "e" 'helm-find-files
    "E" 'helm-find
    "g" 'helm-grep-git-or-ag
    "G" 'helm-grep-git-all-or-ag
    ;; "ha" 'helm-apropos
    ;; "q" 'read-only-mode ; Bad cause in wdired, it's a different mapping.
    "r" 'helm-resume)

  ;; Should we stick to M-based bindings or use C-based?
  ;; Magit uses C-jk, helm uses C-space. Evil has C-w, C-o, C-p, C-r. Emacs has C-xC-q, C-xC-o.
  (define-key helm-map (kbd "M-\\") 'helm-toggle-resplit-and-swap-windows) ; Or use M-t (helm standard binding is C-t).
  (define-key helm-map (kbd "C-f") 'helm-next-page)
  (define-key helm-map (kbd "C-b") 'helm-previous-page)
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

;; Add support for magit.
(require 'evil-magit nil t)
;; C-j/k is the default, M-j/k is more intuitive if we use it for helm.
(when (and (require 'magit-mode nil t) (require 'evil-magit nil t))
  (evil-magit-define-key evil-magit-state 'magit-mode-map "M-j" 'magit-section-forward)
  (evil-magit-define-key evil-magit-state 'magit-mode-map "M-k" 'magit-section-backward))

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

(add-hook
 'minibuffer-setup-hook
 '(lambda ()
    (set (make-local-variable 'evil-echo-state) nil)
    ;; (evil-set-initial-state 'mode 'insert) is the evil-proper
    ;; way to do this, but the minibuffer doesn't have a mode.
    ;; The alternative is to create a minibuffer mode (here), but
    ;; then it may conflict with other packages' if they do the same.
    (evil-insert 1)))
;; Because of the above minibuffer-setup-hook, some bindings need be reset.
(evil-define-key 'normal evil-ex-completion-map [escape] 'abort-recursive-edit)
(evil-define-key 'insert evil-ex-completion-map "\M-p" 'previous-complete-history-element)
(evil-define-key 'insert evil-ex-completion-map "\M-n" 'next-complete-history-element)
;; TODO: evil-ex history binding in normal mode do not work.
(evil-define-key 'normal evil-ex-completion-map "\M-p" 'previous-history-element)
(evil-define-key 'normal evil-ex-completion-map "\M-n" 'next-history-element)
(define-key evil-ex-completion-map "\M-p" 'previous-history-element)
(define-key evil-ex-completion-map "\M-n" 'next-history-element)

;; Remap org-mode meta keys for convenience
;; - org-evil: Not as polished as of May 2017.
;; - evil-org: Depends on MELPA's org-mode, too big a dependency for me.
;; See https://github.com/Somelauw/evil-org-mode/blob/master/doc/keythemes.org for inspiration.
(when (require 'evil-leader nil t)
  (evil-leader/set-key-for-mode 'org-mode "a" 'org-agenda))
(evil-define-key 'normal org-mode-map
  (kbd "M-<return>") (lambda () (interactive) (evil-insert 1) (org-meta-return))
  "\M-l" 'org-shiftright
  "\M-h" 'org-shiftleft
  "\M-k" 'org-shiftup
  "\M-j" 'org-shiftdown
  "\M-L" 'org-shiftmetaright
  "\M-H" 'org-shiftmetaleft
  "\M-K" 'org-shiftmetaup
  "\M-J" 'org-shiftmetadown
  "<" 'org-up-element)

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
;; This shadows evil-magit's "gr", but we can use "?g" for that instead.
;; It shadows C-n/p (`evil-paste-pop'), but we use `helm-show-kill-ring' on
;; another binding.
(when (require 'evil-mc nil t)
  (global-evil-mc-mode 1)
  (define-key evil-mc-key-map (kbd "C-<mouse-1>") 'evil-mc-toggle-cursor-on-click)
  ;; Don't shadow evil's standard keys. TODO: Is \C-t ever needed? Evil's normal binding is `pop-tag-mark'.
  ;; (evil-define-key '(normal visual) evil-mc-key-map "\C-t" nil)

  (set-face-attribute 'evil-mc-cursor-default-face nil :inherit nil :inverse-video nil :box "white")
  (when (require 'evil-mc-extras nil t)
    (global-evil-mc-extras-mode 1)))

(provide 'tool-evil)
