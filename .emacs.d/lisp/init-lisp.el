;;; Lisp

(with-eval-after-load 'lispyville
  ;; REVIEW: lispy-occur: helm-occur does not restrict to region.  Check latest helm.
  (lispyville-set-key-theme
   '(operators            ; Add equivalent for lispy-delete?
     c-w                  ; Bind M-backspace to lispyville-delete-backward-word?
     (escape insert)
     slurp/barf-cp
     ;; (mark insert)
     mark-toggle                        ; TODO: Check out readme.
     ))
  (lispyville--define-key '(motion normal visual)
    (kbd "^") #'lispy-left
    (kbd "M-h") #'lispyville-previous-opening
    (kbd "M-l") #'lispyville-next-opening
    (kbd "M-j") #'lispy-down
    (kbd "M-k") #'lispy-up
    (kbd "M-H") #'lispy-up-slurp        ; lispy-down-slurp?
    (kbd "M-J") #'lispyville-drag-forward
    (kbd "M-K") #'lispyville-drag-backward
    (kbd "M-L") #'lispy-move-right      ; lispy-up-slurp?
    (kbd "C-x C-e") #'lispy-eval
    (kbd "C-j") #'lispy-split
    (kbd "S-C-j") #'lispy-join
    (kbd "C-1") #'lispy-describe-inline
    (kbd "C-2") #'lispy-arglist-inline
    (kbd "C-4") #'lispy-x
    (kbd "gd") #'lispy-goto-symbol
    ;; (kbd "/") #'lispy-occur
    ;; (kbd "M-;") #'lispy-comment ; This conflicts with `iedit-toggle-selection' default binding.
    ;; TODO: lispy-eval-and-replace
    ")" #'lispy-right
    "=" #'lispyville-prettify)
  (lispyville--define-key 'insert
    (kbd "<backspace>") 'lispy-delete-backward
    ";" 'lispy-comment
    ":" 'lispy-colon
    "'" 'lispy-tick
    "`" 'lispy-backtick
    "\"" 'lispy-quotes
    "(" 'lispy-parens
    ")" 'lispy-right-nostring)
  (lispyville--define-key '(motion normal)
    ;; "q" 'lispy-ace-paren              ; REVIEW: Conflicts with magit-blame's quit.  Fixed?
    "Q" 'special-lispy-teleport         ; TODO: Go to closest parenthesis if not on one.
    "q" 'lispy-ace-paren
    ;; "f" 'lispy-ace-paren
    ;; "Q" 'lispy-ace-symbol
    ;; "t" 'lispy-ace-char
    "Y" 'lispy-new-copy
    (kbd "S-<return>") 'lispy-eval-other-window
    ;; "p" 'lispy-paste
    (kbd "M-C") 'lispy-clone            ; TODO: Go to closest parenthesis if not on one.  forward-char + lispyville-backward-up-list?
    "D" 'lispy-kill)

  (lispy-define-key lispy-mode-map-special "C" 'lispy-clone))

(defun ambrevar/init-lispy ()
  (when (require 'lispy nil t)
    (set-face-foreground 'lispy-face-hint "#FF00FF")
    (when (require 'lispyville nil t)
      (add-hook 'lispy-mode-hook 'lispyville-mode))
    (lispyville-mode)))

(dolist (hook '(lisp-mode-hook
                common-lisp-mode-hook
                emacs-lisp-mode-hook
                scheme-mode-hook))
  (add-hook hook 'ambrevar/turn-on-prettify-before-save)
  (add-hook hook 'ambrevar/turn-on-complete-filename)
  (add-hook hook 'ambrevar/turn-on-tab-width-to-8) ; Because some existing code uses tabs.
  (add-hook hook 'ambrevar/turn-off-indent-tabs)   ; Should not use tabs.
  (add-hook hook 'ambrevar/init-lispy)
  (when (fboundp 'rainbow-delimiters-mode)
    (add-hook hook #'rainbow-delimiters-mode)))

(when (require 'rainbow-delimiters nil t)
  ;; See https://yoo2080.wordpress.com/2013/09/08/living-with-rainbow-delimiters-mode/.
  ;; TODO: The color saturation metioned in the URL fails when running in daemon mode.
  ;; https://github.com/Fanael/rainbow-delimiters/issues/36
  (set-face-foreground 'rainbow-delimiters-depth-1-face "#fe1717")
  (set-face-foreground 'rainbow-delimiters-depth-2-face "#589cff")
  (set-face-foreground 'rainbow-delimiters-depth-3-face "#f1fe52")
  (set-face-foreground 'rainbow-delimiters-depth-4-face "#44ff4c")
  (set-face-foreground 'rainbow-delimiters-depth-5-face "#83b2ff")
  (set-face-foreground 'rainbow-delimiters-depth-6-face "#6161ff")
  (set-face-foreground 'rainbow-delimiters-depth-7-face "#35ff35")
  (set-face-foreground 'rainbow-delimiters-depth-8-face "#7ca8ff")
  (set-face-foreground 'rainbow-delimiters-depth-9-face "#50fec1")
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                      :foreground 'unspecified
                      :inherit 'error
                      :strike-through t))

;;; Common LISP.
(setq inferior-lisp-program "clisp")    ; TODO: Use sbcl instead?

;;; Scheme.
(setq geiser-repl-skip-version-check-p t
      geiser-mode-start-repl-p t)

(provide 'init-lisp)
