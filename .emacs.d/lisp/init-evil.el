;; Evil

;; TODO: helm-show-yank-ring behaves like Emacs when pasting whole lines, not like Vim.

;; TODO: helm-mark-ring seems to have issues with Evil:
;; - The first entry is not the last position but the current one.
;; - Navigating through the marks randomly produces a "Marker points into wrong buffer" error.
;; https://github.com/emacs-evil/evil/issues/845#issuecomment-306050231

;; TODO: Make Evil commands react more dynamically with read-only text.
;; Add support for I, C, D, S, s, c*, d*, R, r.
;; See https://github.com/emacs-evil/evil/issues/852.

;; Several packages handle relative line numbering:
;; - nlinum-relative: Seems slow as of May 2017.
;; - linum-relative: integrates well but not with fringe string, must be a function.
;; - relative-line-number: linum must be disabled before running this.
(when (require 'linum-relative nil t)
  ;; REVIEW: Current symbol is displayed on all lines when we run `occur', `set-variables',
  ;; `helm-occur', etc: https://github.com/coldnew/linum-relative/issues/40.
  (setq linum-relative-current-symbol "")
  (linum-relative-toggle))

(evil-mode 1)
(remove-hook 'evil-insert-state-exit-hook 'expand-abbrev)
;; (setq evil-want-abbrev-expand-on-insert-exit nil)
(setq undo-tree-mode-lighter "")

;; The evil-leader package has that over regular bindings that it centralizes
;; the leader key configuration and automatically makes it available in relevant
;; states.  It is not really needed with EXWM however.
(when (require 'evil-leader nil t) (require 'init-evil-leader))

;; Commenting.
;; M-; comments next line in VISUAL. This is because of a different newline
;; definition between Emacs and Vim.
;; https://github.com/redguardtoo/evil-nerd-commenter: does not work well with
;; motions and text objects, e.g. it cannot comment up without M--.
;; `evil-commentary' is the way to go. We don't need an additional minor-mode though.
(when (require 'evil-commentary nil t)
  (evil-global-set-key 'normal "gc" 'evil-commentary)
  (evil-global-set-key 'normal "gy" 'evil-commentary-yank))

;;; Term mode should be in emacs state. It confuses 'vi' otherwise.
;;; Upstream will not change this:
;;; https://github.com/emacs-evil/evil/issues/854#issuecomment-309085267
(evil-set-initial-state 'term-mode 'emacs)

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

;; Change mode-line color by Evil state.
(setq evil-default-modeline-color (cons (face-background 'mode-line) (or (face-foreground 'mode-line) "black")))
(defun evil-color-modeline ()
  (let ((color (cond ((minibufferp) evil-default-modeline-color)
                     ((evil-insert-state-p) '("#006fa0" . "#ffffff")) ; 00bb00
                     ((evil-emacs-state-p)  '("#444488" . "#ffffff"))
                     (t evil-default-modeline-color))))
    (set-face-background 'mode-line (car color))
    (set-face-foreground 'mode-line (cdr color))))
(add-hook 'post-command-hook 'evil-color-modeline)
(setq evil-mode-line-format nil)

;; Add defun text-object.
;; https://github.com/emacs-evil/evil/issues/874
(evil-define-text-object evil-a-defun (count &optional beg end type)
  "Select a defun."
  (evil-select-an-object 'evil-defun beg end type count))
(evil-define-text-object evil-inner-defun (count &optional beg end type)
  "Select inner defun."
  (evil-select-inner-object 'evil-defun beg end type count))
(define-key evil-outer-text-objects-map "d" 'evil-a-defun)
(define-key evil-inner-text-objects-map "d" 'evil-inner-defun)

;; Without the hook, the Edebug keys (f, n, i, etc.) would get mixed up on initialization.
(add-hook 'edebug-mode-hook 'evil-normalize-keymaps)



(require 'init-evil-info)
(require 'init-evil-help)

(with-eval-after-load 'transmission (require 'init-evil-transmission))

(with-eval-after-load 'elfeed (require 'init-evil-elfeed))

;;; Emms: It is important to set the bindings after emms-browser has loaded,
;;; since the mode-maps are defconst'd.
(with-eval-after-load 'emms-browser (require 'init-evil-emms))

(with-eval-after-load 'mu4e
  (when (require 'evil-mu4e nil t)
    (evil-set-initial-state 'mu4e-compose-mode 'insert)))

(with-eval-after-load 'init-helm (require 'init-evil-helm))

(with-eval-after-load 'calendar (require 'init-evil-calendar))

;; nXML
(evil-define-key 'normal nxml-mode-map "<" 'nxml-backward-up-element)

(with-eval-after-load 'magit
  (when (require 'evil-magit nil t)
    (evil-magit-define-key evil-magit-state 'magit-mode-map "<" 'magit-section-up)
    ;; C-j/k is the default, M-j/k is more consistent with our customization for Helm.
    (evil-magit-define-key evil-magit-state 'magit-mode-map "M-j" 'magit-section-forward)
    (evil-magit-define-key evil-magit-state 'magit-mode-map "M-k" 'magit-section-backward)))

(require 'evil-ediff nil t)

(with-eval-after-load 'org (require 'init-evil-org))

(with-eval-after-load 'package (require 'init-evil-package))

(with-eval-after-load 'eshell (require 'init-evil-eshell))

(with-eval-after-load 'pdf-view (require 'init-evil-pdf))

(require 'init-evil-term)

(provide 'init-evil)
