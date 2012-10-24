;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; THEME
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; General
(set-face-foreground  'link              "brightblue" ) 
(set-face-underline-p 'link              t)
(set-face-foreground  'minibuffer-prompt "brightcyan" ) 
(set-face-background  'region            "color-17")
(set-face-foreground  'error             "red")
(set-face-bold-p      'error             t)
(set-face-background  'shadow            "color-234" ) ;; For line numbers.


;; FIXME: fix C functions color.
(font-lock-add-keywords
 'c-mode
 '(
   ("&" . font-lock-keyword-face)
   ("\\<\\(and\\|or\\|not\\)\\>" . font-lock-keyword-face)
   ))

;; Make emacs and mutt colors fit.
(font-lock-add-keywords
 'mail-mode
 '(
      ("^From:" . font-lock-preprocessor-face)
      ("^Subject:" . font-lock-warning-face)
      ("^In-Reply-To:" . font-lock-builtin-face)
      ;; Mail addresses.
      ("\\([[:alnum:]._-]+@[[:alnum:]]+.[[:alnum:]]+\\)" 1 font-lock-string-face)
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
      ("^--.*\\(\n.*\\)*" . font-lock-comment-face)
      ))

;; General purpose. Hightlight the following:
;; Digits, FIXME, TODO.
;; FIXME: Digits regex are not perfect, and may make emacs slow. Sure?
(mapcar
 (lambda (mode)
   (font-lock-add-keywords
    mode
    '(
      ;; ("[^[:digit:][:space:]][[:space:]]*\\(-\\)[[:digit:]]+" 1 font-lock-constant-face)
      ;; ("\\(0x[[:digit:]a-fA-F]+\\)[^[:alnum:]_]" 1 font-lock-constant-face)
      ;; ("[^[:alnum:]_]\\([[:digit:]]*\\.?[[:digit:]]+\\)[^[:alnum:]_.]" 1 font-lock-constant-face)
      ("\\<\\(FIXME\\):" 1 font-lock-warning-face prepend)
      ("\\<\\(TODO\\):" 1 font-lock-warning-face prepend)
      ("\\<\\(WARNING\\):" 1 font-lock-warning-face prepend)
      )))
 '( text-mode 
    sh-mode  emacs-lisp-mode lua-mode
    c-mode 
    latex-mode html-mode texinfo-mode))

;; C-mode printf highlight.
(defvar font-lock-format-specifier-face		'font-lock-format-specifier-face
  "Face name to use for format specifiers.")

(defface font-lock-format-specifier-face
  '((t (:foreground "OrangeRed1")))
  "Font Lock mode face used to highlight format specifiers."
  :group 'font-lock-faces)

;; FIXME: disable highlighting outside of string.
(add-hook
 'c-mode-common-hook
 (lambda ()
   (font-lock-add-keywords
    nil
    '(("[^%]\\(%\\([[:digit:]]+\\$\\)?[-+' #0*]*\\([[:digit:]]*\\|\\*\\|\\*[[:digit:]]+\\$\\)\\(\\.\\([[:digit:]]*\\|\\*\\|\\*[[:digit:]]+\\$\\)\\)?\\([hlLjzt]\\|ll\\|hh\\)?\\([aAbdiuoxXDOUfFeEgGcCsSpn]\\|\\[\\^?.[^]]*\\]\\)\\)"
       1 font-lock-format-specifier-face t)
      ("\\(%%\\)" 
       1 font-lock-format-specifier-face t)) )))

;; FIXME: Does not work.
;; (add-hook
;;  'c-mode-common-hook
;;  (set-face-foreground 'compilation-column-number "magenta")
;; )

;; Old/useless.
;; (set-face-background 'lazy-highlight  "brightgreen" ) 
;; (set-face-background 'secondary-selection "SkyBlue4")
;; (set-face-background 'trailing-whitespace "red1")
;; (set-face-background 'vertical-border  "color-17")
;; (set-face-foreground 'warning  "DarkOrange")
;; (set-face-bold-p 'warning  t)
;; (set-face-foreground 'nobreak-space "cyan")
;; (set-face-foreground 'success "Green1")
;; (set-face-bold-p 'success t)
