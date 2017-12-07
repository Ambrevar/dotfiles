;;; Evil

;;; TODO: helm-show-kill-ring behaves like Emacs when pasting whole lines, not like Vim.

;;; TODO: Make Evil commands react more dynamically with read-only text, like eshell, wdired.
;;; Add support for I, C, D, S, s, c*, d*, R, r.
;;; See https://github.com/emacs-evil/evil/issues/852.

;;; REVIEW: 'cw' fails on the last character of the line when \n does not terminate it.
;;; See https://github.com/emacs-evil/evil/issues/863.

;;; Several packages handle relative line numbering:
;;; - nlinum-relative: Seems slow as of May 2017.
;;; - linum-relative: integrates well but not with fringe string, must be a function.
;;; - relative-line-number: linum must be disabled before running this.
(when (require 'linum-relative nil t)
  ;; REVIEW: Current symbol is displayed on all lines when we run `occur', `set-variables',
  ;; `helm-occur', etc: https://github.com/coldnew/linum-relative/issues/40.
  (setq linum-relative-current-symbol "")
  (linum-relative-toggle))

(evil-mode 1)
(remove-hook 'evil-insert-state-exit-hook 'expand-abbrev)
;; (setq evil-want-abbrev-expand-on-insert-exit nil)
(setq undo-tree-mode-lighter "")

(setq evil-cross-lines t
      evil-move-beyond-eol t ; Especially useful for Edebug.
      evil-move-cursor-back nil
      evil-want-fine-undo t)

(setq-default evil-symbol-word-search t)

;;; Commenting.
;;; M-; comments next line in VISUAL. This is because of a different newline
;;; definition between Emacs and Vim.
;;; https://github.com/redguardtoo/evil-nerd-commenter: does not work well with
;;; motions and text objects, e.g. it cannot comment up without M--.
;;; `evil-commentary' is the way to go. We don't need an additional minor-mode though.
(when (require 'evil-commentary nil t)
  (evil-global-set-key 'normal "gc" 'evil-commentary)
  (evil-global-set-key 'normal "gy" 'evil-commentary-yank))

;;; Term mode should be in emacs state. It confuses 'vi' otherwise.
;;; Upstream will not change this:
;;; https://github.com/emacs-evil/evil/issues/854#issuecomment-309085267
(evil-set-initial-state 'term-mode 'emacs)

;;; For git commit, web edits and others.
;;; Since `with-editor-mode' is not a major mode, `evil-set-initial-state' cannot
;;; be used.
;;; This requires Eshell, shells and more.
(when (require 'with-editor nil t)
  (add-hook 'with-editor-mode-hook 'evil-insert-state))

;;; Go-to-definition.
;;; From https://emacs.stackexchange.com/questions/608/evil-map-keybindings-the-vim-way.
(evil-global-set-key
 'normal "gd"
 (lambda () (interactive)
   (evil-execute-in-emacs-state)
   (call-interactively (key-binding (kbd "M-.")))))

;;; Multiple cursors.
;;; This shadows evil-magit's "gr", but we can use "?g" for that instead.
;;; It shadows C-n/p (`evil-paste-pop'), but we use `helm-show-kill-ring' on
;;; another binding.
(when (require 'evil-mc nil t)
  (global-evil-mc-mode 1)
  (define-key evil-mc-key-map (kbd "C-<mouse-1>") 'evil-mc-toggle-cursor-on-click)
  (set-face-attribute 'evil-mc-cursor-default-face nil :inherit nil :inverse-video nil :box "white")
  (when (require 'evil-mc-extras nil t)
    (global-evil-mc-extras-mode 1)))

;;; Change mode-line color by Evil state.
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

;;; Add defun text-object. TODO: Does not work for "around".
;;; https://github.com/emacs-evil/evil/issues/874
(evil-define-text-object evil-a-defun (count &optional beg end type)
  "Select a defun."
  (evil-select-an-object 'evil-defun beg end type count))
(evil-define-text-object evil-inner-defun (count &optional beg end type)
  "Select inner defun."
  (evil-select-inner-object 'evil-defun beg end type count))
(define-key evil-outer-text-objects-map "d" 'evil-a-defun)
(define-key evil-inner-text-objects-map "d" 'evil-inner-defun)

;;; Without the hook, the Edebug keys (f, n, i, etc.) would get mixed up on initialization.
(add-hook 'edebug-mode-hook 'evil-normalize-keymaps)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mode specific bindings.

(when (require 'evil-collection nil t)
  (evil-collection-init)
  (require 'evil-collection-minibuffer)
  (evil-collection-minibuffer-init))

(with-eval-after-load 'elfeed
  ;; Custom
  (evil-define-key 'motion elfeed-search-mode-map
    (kbd "<return>") 'elfeed-visit-or-play-with-mpv
    "o" 'elfeed-visit-or-play-with-mpv)
  (evil-define-key 'motion elfeed-show-mode-map
    (kbd "<return>") 'elfeed-visit-or-play-with-mpv
    "o" 'elfeed-visit-or-play-with-mpv))

(with-eval-after-load 'init-helm (require 'init-evil-helm))

;;; nXML
;;; TODO: Add to evil-collection?
(evil-define-key 'normal nxml-mode-map
  (kbd "C-j") 'nxml-forward-element
  (kbd "C-k") 'nxml-backward-element
  (kbd "M-j") 'nxml-forward-element ; Custom
  (kbd "M-k") 'nxml-backward-element ; Custom
  ">" 'nxml-down-element
  "<" 'nxml-backward-up-element)
(evil-define-key 'visual nxml-mode-map
  (kbd "C-j") 'nxml-forward-element
  (kbd "C-k") 'nxml-backward-element
  (kbd "M-j") 'nxml-forward-element ; Custom
  (kbd "M-k") 'nxml-backward-element ; Custom
  ">" 'nxml-down-element
  "<" 'nxml-backward-up-element)

(with-eval-after-load 'mu4e
  (when (require 'evil-mu4e nil t)
    ;; TODO: evil-mu4e needs a big overhaul, e.g. 'visual commands are not supported.  Report upstream.
    (evil-define-key 'motion mu4e-headers-mode-map
      "i" 'mu4e-headers-mark-for-flag
      "I" 'mu4e-headers-mark-for-unflag
      ;; "R" 'mu4e-headers-mark-for-refile
      "p" 'mu4e-headers-toggle-include-related
      "r" 'mu4e-compose-reply)
    (evil-define-key 'visual mu4e-headers-mode-map
      "u" 'mu4e-headers-mark-for-unmark)
    (evil-define-key 'motion mu4e-view-mode-map
      (kbd "SPC") 'mu4e-view-scroll-up-or-next
      (kbd "<tab>") 'shr-next-link
      "i" 'mu4e-view-mark-for-flag
      "I" 'mu4e-view-mark-for-unflag
      ;; "R" 'mu4e-view-mark-for-refile
      "r" 'mu4e-compose-reply
      "za" 'mu4e-view-save-attachment-multi
      (kbd "C-j") 'mu4e-view-headers-next
      (kbd "C-k") 'mu4e-view-headers-prev
      (kbd "M-j") 'mu4e-view-headers-next ; Custom
      (kbd "M-k") 'mu4e-view-headers-prev ; Custom
      "h" 'evil-backward-char
      "zh" 'mu4e-view-toggle-html
      "gx" 'mu4e-view-go-to-url)
    (evil-set-initial-state 'mu4e-compose-mode 'insert)))

(with-eval-after-load 'magit
  (when (require 'evil-magit nil t)
    (evil-magit-define-key evil-magit-state 'magit-mode-map "<" 'magit-section-up)
    ;; C-j/k is the default, M-j/k is more consistent with our customization for Helm.
    (evil-magit-define-key evil-magit-state 'magit-mode-map "M-j" 'magit-section-forward)
    (evil-magit-define-key evil-magit-state 'magit-mode-map "M-k" 'magit-section-backward)))

(require 'evil-ediff nil t)

(when (require 'evil-org nil t)
  (with-eval-after-load 'org (require 'init-evil-org))
  (with-eval-after-load 'org-agenda
    (require 'evil-org-agenda)
    (evil-org-agenda-set-keys)))

(with-eval-after-load 'gnus (require 'init-evil-gnus))

(provide 'init-evil)
