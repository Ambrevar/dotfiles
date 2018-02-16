;;; This file enforces consistency in the visual style:
;;; - doc, here-doc, comments, strings are in the same taint.
;;; - search highlight, search lazy follow the same color code.
;;; - diffs (ediff, smerge, etc.) follow the same color code.

;;; To find the variable associated to a currently used color, place the cursor
;;; on it and call `describe-face'. Or browse the `list-faces-display'.

;;; General
(set-face-attribute 'default nil :foreground "white" :background "black")

;;; Font size
(when (fboundp 'tool-bar-mode)
  ;; (set-face-attribute 'default nil :height 100)
  (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-10")))

;;; More readable but more space consuming; try on big screens.
;; (setq-default line-spacing 1)

(set-face-background 'mode-line "white")
;; (set-face-foreground 'link "#00ffff")
(set-face-underline 'link t)
(set-face-foreground 'minibuffer-prompt "#00ffff")
(set-face-background 'region "#191970")
(set-face-attribute 'isearch nil :foreground 'unspecified :background "#2f4f4f" :box "white")
(set-face-attribute 'lazy-highlight nil :inherit 'isearch :foreground 'unspecified :background 'unspecified :box nil)
;;; TODO: Highlight with box does not render well in Sx, ediff, occur, evil-search.
(set-face-attribute 'highlight nil :background 'unspecified :box "white")
(set-face-attribute 'error nil :foreground "red" :weight 'bold)

;;; Cursor type: default (box) is visible and practical.
;; (setq-default cursor-type 'hollow)
(setq-default x-stretch-cursor t)
;;; Blinking cursor is on only when Emacs is not daemonized.
(blink-cursor-mode 0)

;;; Line numbers.
;;; Graphic version has a gray bar separating text from line
;;; numbers, so we can leave the background black.
(if (display-graphic-p)
    (set-face-background 'shadow "black")
  (set-face-background 'shadow "#1c1c1c"))

;;; Whitespace mode
(with-eval-after-load 'whitespace
  (set-face-background 'whitespace-space-after-tab "#a9a9a9")
  (set-face-background 'whitespace-indentation "#696969"))

;;; Programming
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

;;; Compilation mode
(with-eval-after-load 'compile
  (set-face-foreground 'compilation-column-number "cyan")
  (set-face-foreground 'compilation-line-number "cyan"))

;;; Ediff
(with-eval-after-load 'ediff-init
  (set-face-attribute 'ediff-even-diff-A nil :inherit 'ediff-current-diff-A :foreground 'unspecified :background 'unspecified :box nil)
  (set-face-attribute 'ediff-odd-diff-A nil :inherit 'ediff-current-diff-A :foreground 'unspecified :background 'unspecified :box nil)
  (set-face-attribute 'ediff-even-diff-B nil :inherit 'ediff-current-diff-B :foreground 'unspecified :background 'unspecified :box nil)
  (set-face-attribute 'ediff-odd-diff-B nil :inherit 'ediff-current-diff-B :foreground 'unspecified :background 'unspecified :box nil)
  (set-face-attribute 'ediff-even-diff-C nil :inherit 'ediff-current-diff-C :foreground 'unspecified :background 'unspecified :box nil)
  (set-face-attribute 'ediff-odd-diff-C nil :inherit 'ediff-current-diff-C :foreground 'unspecified :background 'unspecified :box nil)
  (set-face-attribute 'ediff-current-diff-A nil :box "white")
  (set-face-attribute 'ediff-current-diff-B nil :box "white")
  (set-face-attribute 'ediff-current-diff-C nil :box "white"))

;;; Outline mode
(with-eval-after-load 'outline
  ;; (set-face-attribute 'outline-1 nil :inherit 'font-lock-warning-face)
  (set-face-attribute 'outline-1 nil :weight 'bold :foreground "#CBAC42")
  (set-face-attribute 'outline-2 nil :weight 'bold :foreground "#7BBF11")
  (set-face-attribute 'outline-3 nil :weight 'bold :foreground "#BC684F")
  (set-face-attribute 'outline-4 nil :weight 'bold :foreground "#4C95BF")
  (set-face-attribute 'outline-5 nil :weight 'bold :foreground "SeaGreen")
  (set-face-attribute 'outline-6 nil :weight 'bold :foreground "DarkSlateGray4")
  (set-face-attribute 'outline-7 nil :weight 'bold :foreground "DarkSlateBlue")
  (set-face-attribute 'outline-8 nil :weight 'bold :foreground "Gold"))


;;; show-paren
(with-eval-after-load 'paren
  ;; (set-face-background 'show-paren-match-face (face-background 'default)) ; Disable background color.
  (set-face-background 'show-paren-match "#555555")
  (set-face-foreground 'show-paren-match "#def")
  (set-face-attribute 'show-paren-match nil :weight 'extra-bold))

;;; Mail mode
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

;;; Key notes highlighting. We need to apply it to the mode hook since
;;; font-lock-add-keywords has no inheritance support.
(set-face-foreground 'font-lock-warning-face "DarkOrange")
(defun ambrevar/fontify-comment-tag ()
  (font-lock-add-keywords
   nil
   ;; See https://en.wikipedia.org/wiki/Comment_(computer_programming)#Tags.
   (mapcar
    (lambda (keyword) `(,(concat "\\<\\(" keyword "\\):") 1 font-lock-warning-face prepend))
    '("FIXME\\(([^)]+)\\)?" "HACK" "OPTIMIZE\\(([^)]+)\\)?" "REVIEW\\(([^)]+)\\)?" "TODO\\(([^)]+)\\)?" "UNDONE" "UX" "WARNING" "XXX"))))
(dolist (hook '(prog-mode-hook text-mode-hook))
  (add-hook hook 'ambrevar/fontify-comment-tag))

;;; Man pages
(with-eval-after-load 'man
  (set-face-attribute 'Man-underline nil :foreground (face-foreground 'font-lock-string-face) :underline nil)
  (set-face-attribute 'Man-overstrike nil :foreground (face-foreground 'font-lock-comment-face) :weight 'normal))

;;; Term
;;; Use lighter blue.
(with-eval-after-load 'ansi-color
  (setf (aref ansi-color-map 34) '(foreground-color . "#1e90ff")))

(provide 'visual)
