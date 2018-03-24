;; Lisp

(with-eval-after-load 'lispyville
  ;; TODO: lispy-occur: helm-occur does not restrict to region.
  (lispyville-set-key-theme
   '(operators            ; Add equivalent for lispy-delete?
     c-w                  ; Bind M-backspace to lispyville-delete-backward-word?
     (escape insert)
     slurp/barf-cp
     ;; (mark insert)
     mark-toggle                        ; TODO: Check out readme.
     ))
  (lispyville--define-key '(motion normal visual)
    (kbd "M-h") #'lispyville-previous-opening
    (kbd "M-l") #'lispyville-next-opening
    (kbd "M-j") #'lispy-down
    (kbd "M-k") #'lispy-up
    (kbd "M-H") #'lispy-up-slurp        ; lispy-down-slurp?
    (kbd "M-J") #'lispyville-drag-forward
    (kbd "M-K") #'lispyville-drag-backward
    (kbd "M-L") #'lispy-move-right      ; lispy-up-slurp?
    (kbd "C-x C-e") #'lispy-eval
    (kbd "=") #'lispyville-prettify
    ;; (kbd "M-;") #'lispy-comment ; This conflicts with `iedit-toggle-selection' default binding.
    )
  (lispyville--define-key '(motion normal)
    "q" 'lispy-ace-paren              ; TODO: Conflicts with magit-blame's quit.
    "Q" 'lispy-ace-symbol
    "Y" 'lispy-new-copy
    "D" 'lispy-kill))

(defun ambrevar/init-lispy ()
  (when (require 'lispy nil t)
    (set-face-foreground 'lispy-face-hint "#FF00FF")
    (when (require 'lispyville nil t)
      (add-hook 'lispy-mode-hook 'lispyville-mode))
    (lispy-mode)))

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

;; Color parentheses.
(when (require 'rainbow-delimiters nil t)
  ;; https://yoo2080.wordpress.com/2013/09/08/living-with-rainbow-delimiters-mode/
  (defvar my-paren-dual-colors
    '("hot pink" "dodger blue"))
  (setq rainbow-delimiters-outermost-only-face-count 0)
  (setq rainbow-delimiters-max-face-count 2)
  (set-face-foreground 'rainbow-delimiters-depth-1-face
                       (elt my-paren-dual-colors 1))
  (set-face-foreground 'rainbow-delimiters-depth-2-face
                       (elt my-paren-dual-colors 0))

  ;; TODO: The following fails when running in daemon mode.
  ;; https://github.com/Fanael/rainbow-delimiters/issues/36
  ;; (require 'cl-lib)
  ;; (require 'color)
  ;; (cl-loop
  ;;  for index from 1 to rainbow-delimiters-max-face-count
  ;;  do
  ;;  (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
  ;;    (cl-callf color-saturate-name (face-foreground face) 30)))

  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                      :foreground 'unspecified
                      :inherit 'error
                      :strike-through t))

;;; Common LISP.
(setq inferior-lisp-program "clisp")    ; TODO: Use sbcl instead?

(provide 'init-lisp)
