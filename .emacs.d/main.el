;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MAIN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;==============================================================================
;; Bindings
;;==============================================================================

;; We use this minor mode to store custom bindings and use them where we want.
(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " my-keys" 'my-keys-minor-mode-map)

(my-keys-minor-mode 1)

(defun my-minibuffer-setup-hook ()
  (my-keys-minor-mode 0))

(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)

;;==============================================================================

;; Toggle line numbers.
(global-set-key (kbd "C-<f5>") 'linum-mode)

;; Copy/Paste to/from clipboard.
;; (define-key my-keys-minor-mode-map (kbd "M-p") (kbd "C-u M-! xclip <SPC> -o"))
;; (define-key my-keys-minor-mode-map (kbd "C-<f6>") (kbd "M-| xsel <SPC> -p <SPC> -i"))
(define-key my-keys-minor-mode-map (kbd "C-<f6>") (kbd "M-| xclip"))
(define-key my-keys-minor-mode-map (kbd "C-<f7>") (kbd "C-u M-! xclip <SPC> -o"))
(define-key my-keys-minor-mode-map (kbd "C-<f8>") (kbd "C-u M-! xclip <SPC> -o <SPC> -selection <SPC> clipboard"))

;; Compilation
(define-key my-keys-minor-mode-map (kbd "C-<f9>") 'compile)
;; (define-key my-keys-minor-mode-map (kbd "<f12>") 'next-error)

;; Window resize
;; TODO: Use more { and } to continue.
(define-key my-keys-minor-mode-map (kbd "C-x {")  (lambda () (interactive) (shrink-window-horizontally 5)))
(define-key my-keys-minor-mode-map (kbd "C-x }")  (lambda () (interactive) (enlarge-window-horizontally 5)))
;; (define-key my-keys-minor-mode-map (kbd "S-C-<down>") 'shrink-window)
;; (define-key my-keys-minor-mode-map (kbd "S-C-<up>") 'enlarge-window)

(define-key my-keys-minor-mode-map (kbd "M-a") 'beginning-of-defun)
(define-key my-keys-minor-mode-map (kbd "M-e") 'end-of-defun)

;; Modern scrolling
(global-set-key [next]
                (lambda () (interactive)
                  (condition-case nil (scroll-up)
                    (end-of-buffer (goto-char (point-max))))))

(global-set-key [prior]
                (lambda () (interactive)
                  (condition-case nil (scroll-down)
                    (beginning-of-buffer (goto-char (point-min))))))

;(global-set-key (kbd "C-<tab>") 'dabbrev-expand)
;(define-key minibuffer-local-map (kbd "C-<tab>") 'dabbrev-expand)

;;==============================================================================
;; General
;;==============================================================================

;; Make questions less annoying
(defalias 'yes-or-no-p 'y-or-n-p)

;; Print column number
(column-number-mode 1)

;; Kill whole line
(setq kill-whole-line t)

;; Line numbers
(autoload 'linum-mode "linum" "toggle line numbers on/off" t)
;; (setq linum-format "%-4d ")
(setq linum-format "%d ")
(global-linum-mode 1) ;; FIXME: This may generate warnings. Bug?

;; Indentation
;(setq standard-indent 4) ;; Set standard indent to 2 rather that 4
(setq-default tab-width 4) ; Tab width set to 2 spaces
(setq-default indent-tabs-mode nil) ; Indentation cannot insert tabs
(setq c-default-style "linux" c-basic-offset 4) ;; Identation style

;; Line by line scrolling
(setq scroll-step 1)

;; Highlight selections -- not activated by default on old emacs.
(transient-mark-mode 1)

;; Mousewheel scrolling -- Does not work?
;(mouse-wheel-mode t)

;; No trailing whitespace
;; WARNING: this can break some configuration files needing whitespaces at the
;; end.
;; (add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Abbreviation support
(setq default-abbrev-mode t)

;; Remember last cursor position
(setq save-place-file "~/.emacs.d/.saveplace")
(setq-default save-place t)
(require 'saveplace)

;; Disable autosave features
(setq auto-save-default nil)

;; Place Backup Files in Specific Directory
(setq backup-inhibited t) ;; Disable backup files.
;(setq make-backup-files t) ;; Enable backup files.
;(setq version-control t) ;; Enable versioning with default values (keep five last versions, I think!)
;(setq backup-directory-alist (quote ((".*" . "~/.emacs.d/.backups/")))) ;; Save all backup file in this directory.

;; Remove menu-bar
(menu-bar-mode -1)

;; Set Fill Column
(setq-default fill-column 80)
;; (setq auto-fill-mode 1) ;; Does not work ?
(add-hook 'text-mode-hook 'turn-on-auto-fill)
;; (add-hook 'c-mode-common-hook 
;;           (lambda ()
;;             (auto-fill-mode 1)
;;             (set (make-local-variable 'fill-nobreak-predicate)
;;                  (lambda ()
;;                    (not (eq (get-text-property (point) 'face)
;;                             'font-lock-comment-face ))
;;                    ))
;;             ))


;; Man-mode
(setenv "MANWIDTH" "80")

;; Windmove mode
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; Browser
(setq browse-url-generic-program (executable-find "dwb")
browse-url-browser-function 'browse-url-generic)

;; Default ispell dictionnay
(setq ispell-dictionary "fr")

;; Use color escape sequences. Only use if needed.
;; (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
