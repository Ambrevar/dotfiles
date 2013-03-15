;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; THEME
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; To find the variable associated to a currently used color, place the cursor
;; on it and call 'customize-face'.

;; General
(set-face-foreground  'link                        "brightblue" )
(set-face-underline-p 'link                        t)
(set-face-foreground  'minibuffer-prompt           "brightcyan" )
(set-face-background  'region                      "color-235")
(set-face-foreground  'error                       "red")
(set-face-bold-p      'error                       t)
(set-face-background  'shadow                      "color-234" ) ;; For line numbers.
(set-face-background  'isearch                     "color-17" )
(set-face-foreground  'isearch                     nil )
(set-face-background  'isearch-lazy-highlight-face "color-237" )

;; Programming
(set-face-foreground  'font-lock-builtin-face           "color-75" )
(set-face-bold-p      'font-lock-builtin-face           t )
(set-face-foreground  'font-lock-comment-delimiter-face "color-242" )
(set-face-foreground  'font-lock-comment-face           "color-242" )
(set-face-foreground  'font-lock-constant-face          "color-105" )
(set-face-foreground  'font-lock-doc-face               "color-28" )
(set-face-foreground  'font-lock-function-name-face     "color-75" )
(set-face-bold-p      'font-lock-function-name-face     t )
(set-face-foreground  'font-lock-keyword-face           "brightred" )
(set-face-bold-p      'font-lock-keyword-face           t )
(set-face-foreground  'font-lock-preprocessor-face      "color-99" )
(set-face-foreground  'font-lock-string-face            "color-39" )
(set-face-foreground  'font-lock-type-face              "color-166" )
(set-face-foreground  'font-lock-variable-name-face     "brightyellow" )
(set-face-foreground  'font-lock-warning-face           "DarkOrange" )

;; Ediff
(add-hook
 'ediff-mode-hook
 (lambda ()
   (set-face-background 'ediff-fine-diff-A "color-52")
   (set-face-foreground 'ediff-fine-diff-A nil)
   (set-face-background 'ediff-fine-diff-B "color-52")
   (set-face-foreground 'ediff-fine-diff-B nil)
   (set-face-background 'ediff-fine-diff-C "color-52")
   (set-face-foreground 'ediff-fine-diff-C nil)

   (set-face-background 'ediff-current-diff-A "color-17")
   (set-face-foreground 'ediff-current-diff-A nil)
   (set-face-background 'ediff-current-diff-B "color-17")
   (set-face-foreground 'ediff-current-diff-B nil)
   (set-face-background 'ediff-current-diff-C "color-17")
   (set-face-foreground 'ediff-current-diff-C nil)

   (set-face-background 'ediff-even-diff-A "color-236")
   (set-face-foreground 'ediff-even-diff-A nil)
   (set-face-background 'ediff-even-diff-B "color-236")
   (set-face-foreground 'ediff-even-diff-B nil)
   (set-face-background 'ediff-even-diff-C "color-236")
   (set-face-foreground 'ediff-even-diff-C nil)

   (set-face-background 'ediff-odd-diff-A "brightblack")
   (set-face-foreground 'ediff-odd-diff-A nil)
   (set-face-background 'ediff-odd-diff-B "brightblack")
   (set-face-foreground 'ediff-odd-diff-B nil)
   (set-face-background 'ediff-odd-diff-C "brightblack")
   (set-face-foreground 'ediff-odd-diff-C nil)))

;; Show paren.
(set-face-background 'show-paren-match-face (face-background 'default))
(set-face-foreground 'show-paren-match-face "#def")
(set-face-attribute 'show-paren-match-face nil :weight 'extra-bold)

;; Make Emacs and Mutt colors fit.
(font-lock-add-keywords
 'mail-mode
 '(
      ("^From:" . font-lock-preprocessor-face)
      ("^Subject:" . font-lock-warning-face)
      ("^In-Reply-To:" . font-lock-builtin-face)
      ;; Mail addresses.
      ("\\([[:alnum:]._-]+@[[:alnum:]._-]+\.[[:alnum:]._-]+\\)" 1 font-lock-string-face)
      ;; Quote
      ("^\> *\\([^\> ]\\).*$" . font-lock-doc-face)
      ;; Quote1
      ("^\> *\> *\\([^\> ]\\).*$" . font-lock-constant-face)
      ("^\> *\> *\> *\\([^\> ]\\).*$" . font-lock-type-face)
      ("^\> *\> *\> *\> *\\([^\> ]\\).*$" . font-lock-variable-name-face)
      ("^\> *\> *\> *\> *\> *\\([^\> ]\\).*$" . font-lock-comment-face)
      ("^\> *\> *\> *\> *\> *\> *\\([^\> ]\\).*$" . font-lock-comment-face)
      ("^\> *\> *\> *\> *\> *\> *\> *\\([^\> ]\\).*$" . font-lock-comment-face)
      ("^\> *\> *\> *\> *\> *\> *\> *\> *\\([^\> ]\\).*$" . font-lock-comment-face)
      ;; Signature
      ;; TODO: does not work properly.
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
 '(lua-mode-hook prog-mode-hook tex-mode-hook))
;; Digits regex are not perfect, and may make emacs slow. Sure?
;; ("[^[:digit:][:space:]][[:space:]]*\\(-\\)[[:digit:]]+" 1 font-lock-constant-face)
;; ("\\(0x[[:digit:]a-fA-F]+\\)[^[:alnum:]_]" 1 font-lock-constant-face)
;; ("[^[:alnum:]_]\\([[:digit:]]*\\.?[[:digit:]]+\\)[^[:alnum:]_.]" 1 font-lock-constant-face)

;; C '&' address as keyword.
(font-lock-add-keywords
 'c-mode
 '(("&" . font-lock-keyword-face)
   ("\\<\\(and\\|or\\|not\\)\\>" . font-lock-keyword-face)))



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

;; TODO: Does not work.
;; (add-hook
;;  'c-mode-common-hook
;;  (set-face-foreground 'compilation-column-number "magenta")
;; )
