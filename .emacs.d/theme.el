;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; THEME
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; I've tried to group colors with some consistency:
;; * doc, here-doc, comments, strings
;; * variables, types, constants
;; * preprocessor, functions, keywords, builtins
;; * search highlight, search lazy

;; To find the variable associated to a currently used color, place the cursor
;; on it and call 'describe-face'.

;; General
(set-face-foreground  'default                     "white" )
(set-face-background  'default                     "black" )
;; Font size
(if (fboundp 'tool-bar-mode)
    ;; (set-face-attribute 'default nil :height 100)
    (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-10")))
(if (string-match "^23.*" emacs-version )
    (set-face-background 'modeline "white")
  (set-face-background 'mode-line "white"))

(set-face-foreground  'link                        "#00ffff" )
(set-face-underline-p 'link                        t)
(set-face-foreground  'minibuffer-prompt           "#00ffff" )
(set-face-background  'region                      "#191970")
(set-face-background  'isearch                     "#8b0000" )
(set-face-foreground  'isearch                     "white" )
(set-face-background  'isearch-lazy-highlight-face "#8b4500" )
(set-face-foreground  'isearch-lazy-highlight-face "white" )
(when (>= emacs-major-version 24)
      (set-face-foreground  'error                       "red")
      (set-face-bold-p      'error                       t))

;; Blinking cursor is on only when Emacs is not daemonized.
(when (not (boundp 'server-running-p))
  (blink-cursor-mode -1))

;; Line numbers. Graphic version has a gray bar separating text from line
;; numbers, so we can leave the background black.
(if (display-graphic-p)
    (set-face-background  'shadow                      "black" )
  (set-face-background  'shadow                      "#1c1c1c" ))

;; Programming
(set-face-foreground  'font-lock-builtin-face           "#d2691e" )
(set-face-bold-p      'font-lock-builtin-face           t )
(set-face-foreground  'font-lock-comment-delimiter-face "#008b8b" )
(set-face-foreground  'font-lock-comment-face           "#008b8b" )
(set-face-foreground  'font-lock-constant-face          "#d2691e" )
(set-face-foreground  'font-lock-doc-face               "#00bfff" )
(set-face-foreground  'font-lock-function-name-face     "#1e90ff" )
(set-face-bold-p      'font-lock-function-name-face     t )
(set-face-foreground  'font-lock-keyword-face           "#3cb371" )
(set-face-bold-p      'font-lock-keyword-face           t )
(set-face-foreground  'font-lock-preprocessor-face      "#9400d3" )
(set-face-foreground  'font-lock-string-face            "#0080d4" )
(set-face-foreground  'font-lock-type-face              "#aa2a00" )
(set-face-foreground  'font-lock-variable-name-face     "#ffff00" )
(set-face-foreground  'font-lock-warning-face           "DarkOrange" )

;; Sh-mode
(add-hook
 'sh-mode-hook
 (lambda ()
   (set-face-foreground 'sh-heredoc "#00bfff")
   (set-face-bold-p 'sh-heredoc nil)))

;; Eshell
(add-hook
 'eshell-mode-hook
 (lambda ()
   (set-face-foreground 'eshell-prompt "#008b8b")))

(add-hook
 'compilation-mode-hook
 (lambda ()
   (set-face-foreground 'compilation-column-number "cyan")
   (set-face-foreground 'compilation-line-number "cyan")
   (set-face-foreground 'compilation-error "red")
   (set-face-foreground 'compilation-info "green")
))

;; C additional keywords.
(font-lock-add-keywords
 'c-mode
 '(("&" . font-lock-keyword-face)
   ("\\<\\(and\\|or\\|not\\)\\>" . font-lock-keyword-face)))

;; Ediff
(add-hook
 'ediff-mode-hook
 (lambda ()
   (set-face-background 'ediff-fine-diff-A "#2a0000")
   (set-face-foreground 'ediff-fine-diff-A nil)
   (set-face-background 'ediff-fine-diff-B "#2a0000")
   (set-face-foreground 'ediff-fine-diff-B nil)
   (set-face-background 'ediff-fine-diff-C "#2a0000")
   (set-face-foreground 'ediff-fine-diff-C nil)

   (set-face-background 'ediff-current-diff-A "#00002a")
   (set-face-foreground 'ediff-current-diff-A nil)
   (set-face-background 'ediff-current-diff-B "#00002a")
   (set-face-foreground 'ediff-current-diff-B nil)
   (set-face-background 'ediff-current-diff-C "#00002a")
   (set-face-foreground 'ediff-current-diff-C nil)

   (set-face-background 'ediff-even-diff-A "#303030")
   (set-face-foreground 'ediff-even-diff-A nil)
   (set-face-background 'ediff-even-diff-B "#303030")
   (set-face-foreground 'ediff-even-diff-B nil)
   (set-face-background 'ediff-even-diff-C "#303030")
   (set-face-foreground 'ediff-even-diff-C nil)

   (set-face-background 'ediff-odd-diff-A "brightblack")
   (set-face-foreground 'ediff-odd-diff-A nil)
   (set-face-background 'ediff-odd-diff-B "brightblack")
   (set-face-foreground 'ediff-odd-diff-B nil)
   (set-face-background 'ediff-odd-diff-C "brightblack")
   (set-face-foreground 'ediff-odd-diff-C nil)))

;; Show paren.
(when (boundp 'show-paren-delay)
  (set-face-background 'show-paren-match-face (face-background 'default))
  (set-face-foreground 'show-paren-match-face "#def")
  (set-face-attribute 'show-paren-match-face nil :weight 'extra-bold))

;; Make Emacs and Mutt colors fit.
(font-lock-add-keywords
 'mail-mode
 '(
      ("^From:" . font-lock-preprocessor-face)
      ("^Subject:" . font-lock-warning-face)
      ("^Reply-To:" . font-lock-type-face)
      ("^In-Reply-To:" . font-lock-builtin-face)
      ;; Mail addresses.
      ("\\([[:alnum:]._-]+@[[:alnum:]._-]+\.[[:alnum:]._-]+\\)" 1 font-lock-string-face)
      ;; Quote
      ("^\> *\\([^\> ]\\).*$" . font-lock-doc-face)
      ;; Quote+
      ("^\> *\> *\\([^\> ]\\).*$" . font-lock-constant-face)
      ("^\> *\> *\> *\\([^\> ]\\).*$" . font-lock-type-face)
      ("^\> *\> *\> *\> *\\([^\> ]\\).*$" . font-lock-variable-name-face)
      ("^\> *\> *\> *\> *\> *\\([^\> ]\\).*$" . font-lock-comment-face)
      ("^\> *\> *\> *\> *\> *\> *\\([^\> ]\\).*$" . font-lock-comment-face)
      ("^\> *\> *\> *\> *\> *\> *\> *\\([^\> ]\\).*$" . font-lock-comment-face)
      ("^\> *\> *\> *\> *\> *\> *\> *\> *\\([^\> ]\\).*$" . font-lock-comment-face)
      ;; Signature
      ;; TODO: mail signature face does not work properly.
      ("^--.*\\(\n.*\\)*" . font-lock-comment-face)))

;; Key notes highlighting. We need to apply it to the mode hook since
;; font-lock-add-keywords has no inheritance support.
(mapcar
 (lambda (mode-hook)
   (add-hook
    mode-hook
    (lambda () (interactive)
      (font-lock-add-keywords
       nil
       '(("\\<\\(FIXME\\):" 1 font-lock-warning-face prepend)
         ("\\<\\(TODO\\):" 1 font-lock-warning-face prepend)
         ("\\<\\(WARNING\\):" 1 font-lock-warning-face prepend))))))
 '(lua-mode-hook prog-mode-hook tex-mode-hook texinfo-mode-hook))
;; Digits regex are not perfect, and may make emacs slow. Sure?
;; ("[^[:digit:][:space:]][[:space:]]*\\(-\\)[[:digit:]]+" 1 font-lock-constant-face)
;; ("\\(0x[[:digit:]a-fA-F]+\\)[^[:alnum:]_]" 1 font-lock-constant-face)
;; ("[^[:alnum:]_]\\([[:digit:]]*\\.?[[:digit:]]+\\)[^[:alnum:]_.]" 1 font-lock-constant-face)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'theme)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; C-mode printf highlight.
;; (defvar font-lock-format-specifier-face		'font-lock-format-specifier-face
;;   "Face name to use for format specifiers.")
;; (defface font-lock-format-specifier-face
;;   '((t (:foreground "OrangeRed1")))
;;   "Font Lock mode face used to highlight format specifiers."
;;   :group 'font-lock-faces)
;; TODO: disable highlighting outside of string.
;; (add-hook
;;  'c-mode-common-hook
;;  (lambda ()
;;    (font-lock-add-keywords
;;     nil
;;     '(("[^%]\\(%\\([[:digit:]]+\\$\\)?[-+' #0*]*\\([[:digit:]]*\\|\\*\\|\\*[[:digit:]]+\\$\\)\\(\\.\\([[:digit:]]*\\|\\*\\|\\*[[:digit:]]+\\$\\)\\)?\\([hlLjzt]\\|ll\\|hh\\)?\\([aAbdiuoxXDOUfFeEgGcCsSpn]\\|\\[\\^?.[^]]*\\]\\)\\)"
;;        1 font-lock-format-specifier-face t)
;;       ("\\(%%\\)"
;;        1 font-lock-format-specifier-face t)) )))

;; Man pages
;; TODO: man mode does not get colored because the hook does not exist.
;; (make-face 'man-bold-face)
;; (make-face 'man-underline-face)
;; (set-face-foreground 'man-bold-face "red")
;; (set-face-foreground 'man-underline-face "green")
;; (add-hook
;;  'man-mode-hook
;;  (lambda ()
;;    (set (make-local-variable 'bold) 'man-bold-face)
;;    (set (make-local-variable 'underline) 'man-underline-face)))
