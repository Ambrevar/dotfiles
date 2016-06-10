;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; THEME
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; I've tried to group colors with some consistency:
;; * doc, here-doc, comments, strings
;; * variables, types, functions
;; * preprocessor, constants, keywords, builtins
;; * search highlight, search lazy

;; To find the variable associated to a currently used color, place the cursor
;; on it and call 'describe-face'.

;; General
(set-face-foreground  'default                     "white")
(set-face-background  'default                     "black")
;; Font size
(if (fboundp 'tool-bar-mode)
    ;; (set-face-attribute 'default nil :height 100)
    (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-10")))
(if (string-match "^23.*" emacs-version)
    (set-face-background 'modeline "white")
  (set-face-background 'mode-line "white"))

(set-face-foreground  'link                        "#00ffff")
(set-face-underline-p 'link                        t)
(set-face-foreground  'minibuffer-prompt           "#00ffff")
(set-face-background  'region                      "#191970")
(set-face-background  'isearch                     "#8b0000")
(set-face-foreground  'isearch                     "white")
(set-face-background  'isearch-lazy-highlight-face "#8b4500")
(set-face-foreground  'isearch-lazy-highlight-face "white")
(when (>= emacs-major-version 24)
      (set-face-foreground  'error "red")
      (set-face-bold-p      'error t))

;; Blinking cursor is on only when Emacs is not daemonized.
(unless (getenv "EMACS_SERVER")
  (blink-cursor-mode -1))

;; Line numbers. Graphic version has a gray bar separating text from line
;; numbers, so we can leave the background black.
(if (display-graphic-p)
    (set-face-background 'shadow "black")
  (set-face-background 'shadow   "#1c1c1c"))

;; Whitespace mode
(add-hook
 'whitespace-load-hook
 (lambda ()
   (set-face-background 'whitespace-space-after-tab "#a9a9a9")
   (set-face-background 'whitespace-indentation     "#696969")))

;; Programming
(set-face-foreground  'font-lock-builtin-face           "#d2691e")
(set-face-bold-p      'font-lock-builtin-face           t)
(set-face-foreground  'font-lock-comment-delimiter-face "#008b8b")
(set-face-foreground  'font-lock-comment-face           "#008b8b")
(set-face-foreground  'font-lock-constant-face          "#9400d3")
;; (set-face-foreground  'font-lock-constant-face          "#1e90ff")
;; (set-face-bold-p      'font-lock-constant-face          t)
(set-face-foreground  'font-lock-doc-face               "#00bfff")
(set-face-foreground  'font-lock-function-name-face     "#d2691e")
(set-face-foreground  'font-lock-keyword-face           "#3cb371")
(set-face-bold-p      'font-lock-keyword-face           t)
(set-face-foreground  'font-lock-preprocessor-face      "#9400d3")
(set-face-foreground  'font-lock-string-face            "#0080d4")
(set-face-foreground  'font-lock-type-face              "#aa2a00")
(set-face-foreground  'font-lock-variable-name-face     "#ffff00")
(set-face-foreground  'font-lock-warning-face           "DarkOrange")

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
   (set-face-foreground 'compilation-info "green")))

;; C additional keywords.
(mapcar
 (lambda (mode)
   (font-lock-add-keywords
    mode
    '(("\\<\\(and\\|or\\|not\\)\\>" . font-lock-keyword-face)
      ; ("&" . font-lock-keyword-face)
      ;; Colour function calls.
      ("\\<\\(\\sw+\\)(" 1 'font-lock-function-name-face)
      ("\\<\\(\\sw+\\)<\\sw+>(" 1 'font-lock-function-name-face))))
 '(c-mode c++-mode))

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

   (set-face-background 'ediff-odd-diff-A "#696969")
   (set-face-foreground 'ediff-odd-diff-A nil)
   (set-face-background 'ediff-odd-diff-B "#696969")
   (set-face-foreground 'ediff-odd-diff-B nil)
   (set-face-background 'ediff-odd-diff-C "#696969")
   (set-face-foreground 'ediff-odd-diff-C nil)))

;; Show paren.
(when (boundp 'show-paren-delay)
  ;; (set-face-background 'show-paren-match-face (face-background 'default)) ; Disable background color.
  (set-face-background 'show-paren-match-face "#555555")
  (set-face-foreground 'show-paren-match-face "#def")
  (set-face-attribute 'show-paren-match-face nil :weight 'extra-bold))

;; Make Emacs and Mutt colors fit.
(font-lock-add-keywords
 'mail-mode
 '(("^From:" . font-lock-preprocessor-face)
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
 '(prog-mode-hook tex-mode-hook texinfo-mode-hook))
;; Digits regex are not perfect. May make Emacs slow?
;; ("[^[:digit:][:space:]][[:space:]]*\\(-\\)[[:digit:]]+" 1 font-lock-constant-face)
;; ("\\(0x[[:digit:]a-fA-F]+\\)[^[:alnum:]_]" 1 font-lock-constant-face)
;; ("[^[:alnum:]_]\\([[:digit:]]*\\.?[[:digit:]]+\\)[^[:alnum:]_.]" 1 font-lock-constant-face)

;; Man pages
(add-hook
 'Man-mode-hook
 (lambda ()
   (set-face-foreground 'Man-underline "yellow")
   (set-face-foreground 'Man-overstrike "cyan")
   (set-face-bold-p 'Man-overstrike nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'theme)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
