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
;; Should we map <leader<leader> to the most used command, e.g. `helm-mini'?
;; Could be misleading.
(when (require 'evil-leader nil t)
  ;; Leader mode and its key must be set before evil-mode.
  (evil-leader/set-leader "<SPC>")
  (global-evil-leader-mode))

(evil-mode 1)

(defun eshell-or-new-session (&optional arg)
  "Create an interactive Eshell buffer.
If there is already an Eshell session active, switch to it.
If current buffer is already an Eshell buffer, create a new one and switch to it.
See `eshell' for the numeric prefix arg."
  (interactive "P")
  (if (eq major-mode 'eshell-mode)
      (eshell (or arg t))
    (eshell arg)))

(evil-leader/set-key
  "RET" 'eshell-or-new-session
  "\\" 'toggle-window-split
  "b" 'buffer-menu
  "e" 'find-file
  "k" 'kill-this-buffer
  "o" 'delete-other-windows
  "w" 'evil-window-next
  "|" 'swap-windows)
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

;;; Term mode should be in emacs state. It confuses 'vi' otherwise.
;;; Upstream will not change this:
;;; https://github.com/emacs-evil/evil/issues/854#issuecomment-309085267
(evil-set-initial-state 'term-mode 'emacs)

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
  (define-keys helm-map
    "M-\\" 'helm-toggle-resplit-and-swap-windows ; Or use M-t (helm standard binding is C-t).
    "C-f" 'helm-next-page
    "C-b" 'helm-previous-page
    "M-h" 'helm-next-source
    "M-j" 'helm-next-line
    "M-k" 'helm-previous-line
    "M-l" 'helm-execute-persistent-action ;(kbd "RET")
    "M-H" 'describe-key
    "<escape>" 'helm-keyboard-quit)
  (dolist (keymap (list helm-buffer-map))
    (define-key keymap (kbd "M-o") 'helm-buffer-switch-other-window))
  (dolist (keymap (list helm-find-files-map helm-read-file-map))
    (define-keys keymap
      "M-o" 'helm-ff-run-switch-other-window
      "M-." 'helm-ff-run-find-sh-command
      "M-h" 'helm-find-files-up-one-level
      "M-l" 'helm-execute-persistent-action
      "M-H" 'describe-key)))

;; Add support for magit.
(require 'evil-magit nil t)
;; C-j/k is the default, M-j/k is more intuitive if we use it for helm.
(when (and (require 'magit-mode nil t) (require 'evil-magit nil t))
  (evil-magit-define-key evil-magit-state 'magit-mode-map "<" 'magit-section-up)
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
(when (require 'evil-leader nil t)
  (evil-leader/set-key-for-mode 'org-mode "a" 'org-agenda))
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

;; Package-menu mode
(delete 'package-menu-mode evil-emacs-state-modes)
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
  (with-eval-after-load 'tool-helm
    (evil-define-key 'insert eshell-mode-map "\C-e" 'helm-find-files))
  (evil-define-key 'normal eshell-mode-map "\M-k" 'eshell-previous-prompt)
  (evil-define-key 'normal eshell-mode-map "\M-j" 'eshell-next-prompt)
  (evil-define-key 'normal eshell-mode-map "0" 'eshell-bol)
  (evil-define-key 'normal eshell-mode-map (kbd "RET") 'eshell-send-input)
  (evil-define-key 'normal eshell-mode-map (kbd "C-c C-c") 'evil/eshell-interrupt-process)
  (evil-define-key '(normal insert) eshell-mode-map "\M-h" 'eshell-backward-argument)
  (evil-define-key '(normal insert) eshell-mode-map "\M-l" 'eshell-forward-argument))
(add-hook 'eshell-first-time-mode-hook 'evil/eshell-set-keys)

;; TODO: When point is on "> ", helm-find-files looks up parent folder. Prevent that.

;; TODO: Make Evil commands react more dynamically with read-only text.
;; Add support for I, C, D, S, s, c*, d*, R, r.
;; See https://github.com/emacs-evil/evil/issues/852
(defun evil/eshell-delete-whole-line ()
  (interactive)
  (if (not (get-text-property (line-beginning-position) 'read-only))
      (evil-delete-whole-line (line-beginning-position) (line-end-position))
    (eshell-return-to-prompt) ; Difference with eshell-bol?
    (evil-delete-line (point) (line-end-position))))
;; (evil-define-key 'normal eshell-mode-map "dd" 'evil/eshell-delete-whole-line)
(defun evil/eshell-change-whole-line ()
  (interactive)
  (if (not (get-text-property (line-beginning-position) 'read-only))
      (evil-change-whole-line (line-beginning-position) (line-end-position))
    (eshell-return-to-prompt) ; Difference with eshell-bol?
    (evil-change-line (point) (line-end-position))))
;; (evil-define-key 'normal eshell-mode-map "cc" 'evil/eshell-change-whole-line)

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
