;; This file enforces consistency in the visual style:
;; - doc, here-doc, comments, strings are in the same taint.
;; - search highlight, search lazy follow the same color code.
;; - diffs (ediff, smerge, etc.) follow the same color code.

;; To find the variable associated to a currently used color, place the cursor
;; on it and call `describe-face'. Or browse the `list-faces-display'.

;; General
(set-face-attribute 'default nil :foreground "white" :background "black")

;; Font size
(when (fboundp 'tool-bar-mode)
  ;; (set-face-attribute 'default nil :height 100)
  (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-10")))

;; More readable but more space consuming; try on big screens.
; (setq-default line-spacing 1)

(if (= emacs-major-version 23)
    (set-face-background 'modeline "white")
  (set-face-background 'mode-line "white"))

;; (set-face-foreground 'link "#00ffff")
(set-face-underline-p 'link t)
(set-face-foreground 'minibuffer-prompt "#00ffff")
(set-face-background 'region "#191970")
(set-face-attribute 'isearch nil :foreground "#8b0000" :background "white")
(set-face-attribute 'isearch-lazy-highlight-face nil :foreground "#8b4500" :background "white")
(set-face-attribute 'highlight nil :background 'unspecified :box "white")
(when (>= emacs-major-version 24)
  (set-face-attribute 'error nil :foreground "red" :weight 'bold))

;; Cursor type: default (box) is visible and practical.
; (setq-default cursor-type 'hollow)
(setq-default x-stretch-cursor t)
;; Blinking cursor is on only when Emacs is not daemonized.
(blink-cursor-mode 0)

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
   (set-face-background 'whitespace-indentation "#696969")))

;; Programming
(set-face-foreground 'font-lock-comment-face "#00ced1")
(set-face-foreground 'font-lock-comment-delimiter-face (face-foreground 'font-lock-comment-face))
(set-face-foreground 'font-lock-doc-face "#00dfff")
(set-face-foreground 'font-lock-string-face "#0080d4")
(set-face-foreground 'font-lock-builtin-face (face-foreground 'default))
(set-face-foreground 'font-lock-constant-face (face-foreground 'default))
(set-face-foreground 'font-lock-function-name-face (face-foreground 'default))
(set-face-foreground 'font-lock-keyword-face (face-foreground 'default))
(set-face-foreground 'font-lock-type-face (face-foreground 'default))
(set-face-foreground 'font-lock-variable-name-face (face-foreground 'default))

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

;; Compilation mode
(add-hook
 'compilation-mode-hook
 (lambda ()
   (set-face-foreground 'compilation-column-number "cyan")
   (set-face-foreground 'compilation-line-number "cyan")))

;; C additional keywords.
(mapc
 (lambda (mode)
   (font-lock-add-keywords
    mode
    '(("\\<\\(and\\|or\\|not\\)\\>" . font-lock-keyword-face)
      ;; Functions.
      ("\\<\\(\\sw+\\)(" 1 'font-lock-function-name-face)
      ("\\<\\(\\sw+\\)<\\sw+>(" 1 'font-lock-function-name-face))))
 '(c-mode c++-mode))

;; Ediff
(add-hook
 'ediff-mode-hook
 (lambda ()
   (set-face-attribute 'ediff-even-diff-A nil :inherit 'ediff-current-diff-A :foreground 'unspecified :background 'unspecified :box nil)
   (set-face-attribute 'ediff-odd-diff-A nil :inherit 'ediff-current-diff-A :foreground 'unspecified :background 'unspecified :box nil)
   (set-face-attribute 'ediff-even-diff-B nil :inherit 'ediff-current-diff-B :foreground 'unspecified :background 'unspecified :box nil)
   (set-face-attribute 'ediff-odd-diff-B nil :inherit 'ediff-current-diff-B :foreground 'unspecified :background 'unspecified :box nil)
   (set-face-attribute 'ediff-even-diff-C nil :inherit 'ediff-current-diff-C :foreground 'unspecified :background 'unspecified :box nil)
   (set-face-attribute 'ediff-odd-diff-C nil :inherit 'ediff-current-diff-C :foreground 'unspecified :background 'unspecified :box nil)
   (set-face-attribute 'ediff-current-diff-A nil :box "white")
   (set-face-attribute 'ediff-current-diff-B nil :box "white")
   (set-face-attribute 'ediff-current-diff-C nil :box "white")))

;; Outline mode
(add-hook
 'outline-mode-hook
 (lambda ()
   (set-face-attribute 'outline-1 nil :inherit 'font-lock-warning-face)
   (set-face-foreground 'outline-2 "Gold")
   (set-face-foreground 'outline-3 "SpringGreen")
   (set-face-foreground 'outline-4 "LightSlateBlue")
   (set-face-foreground 'outline-5 "SlateGrey")
   (set-face-foreground 'outline-6 "SeaGreen")
   (set-face-foreground 'outline-7 "DarkSlateGray4")
   (set-face-foreground 'outline-8 "DarkSlateBlue")))

;; show-paren
(when (boundp 'show-paren-delay)
  ;; (set-face-background 'show-paren-match-face (face-background 'default)) ; Disable background color.
  (set-face-background 'show-paren-match-face "#555555")
  (set-face-foreground 'show-paren-match-face "#def")
  (set-face-attribute 'show-paren-match-face nil :weight 'extra-bold))

;; Mail mode
(font-lock-add-keywords
 'mail-mode
 '(
   ("^From:" . font-lock-warning-face)
   ("^To:" . font-lock-warning-face)
   ("^Newsgroups:" . font-lock-warning-face)
   ("^B?CC:" . font-lock-warning-face)
   ("^Subject:" . font-lock-warning-face)
   ("^Reply-To:" . font-lock-warning-face)
   ("^In-Reply-To:" . font-lock-warning-face)
   ;; Mail addresses.
   ("\\([[:alnum:]._-]+@[[:alnum:]._-]+\.[[:alnum:]._-]+\\)" 1 font-lock-string-face)
   ;; Quotes.
   ("^\> *\\([^\> ]\\).*$" . font-lock-string-face)
   ("^\> *\> *\\([^\> ]\\).*$" . font-lock-doc-face)
   ("^\> *\> *\> *\\([^\> ]\\).*$" . font-lock-comment-face)
   ("^\> *\> *\> *\> *\\([^\> ]\\).*$" . font-lock-comment-face)
   ("^\> *\> *\> *\> *\> *\\([^\> ]\\).*$" . font-lock-comment-face)
   ("^\> *\> *\> *\> *\> *\> *\\([^\> ]\\).*$" . font-lock-comment-face)
   ("^\> *\> *\> *\> *\> *\> *\> *\\([^\> ]\\).*$" . font-lock-comment-face)
   ("^\> *\> *\> *\> *\> *\> *\> *\> *\\([^\> ]\\).*$" . font-lock-comment-face)
   ;; Signature (multi-line regexes are a bit flaky).
   ("^--.*\\(\n.*\\)*" . font-lock-comment-face)))

;; Key notes highlighting. We need to apply it to the mode hook since
;; font-lock-add-keywords has no inheritance support.
(set-face-foreground 'font-lock-warning-face "DarkOrange")
(mapc
 (lambda (mode-hook)
   (add-hook
    mode-hook
    (lambda () (interactive)
      (font-lock-add-keywords
       nil
       ;; See https://en.wikipedia.org/wiki/Comment_(computer_programming)#Tags.
       '(("\\<\\(FIXME\\(([^)]+)\\)?\\):" 1 font-lock-warning-face prepend)
         ("\\<\\(TODO\\(([^)]+)\\)?\\):" 1 font-lock-warning-face prepend)
         ("\\<\\(UNDONE\\):" 1 font-lock-warning-face prepend)
         ("\\<\\(UX\\):" 1 font-lock-warning-face prepend)
         ("\\<\\(WARNING\\):" 1 font-lock-warning-face prepend)
         ("\\<\\(XXX\\):" 1 font-lock-warning-face prepend))))))
 '(prog-mode-hook tex-mode-hook texinfo-mode-hook))

;; Man pages
(add-hook
 'Man-mode-hook
 (lambda ()
   (set-face-attribute 'Man-underline nil :foreground (face-foreground 'font-lock-string-face) :underline nil)
   (set-face-attribute 'Man-overstrike nil :foreground (face-foreground 'font-lock-comment-face) :weight 'normal)))

(provide 'visual)
