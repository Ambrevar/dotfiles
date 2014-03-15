;;==============================================================================
;; TeX
;;==============================================================================
;; The default tex-mode and AucTeX may seem quite disappointing. Let's use
;; custom KISS functions for everything.

;; Interesting options for the tex compiler:
;; * -file-line-error-style: change the style of error report to
;;    display file name and line first.
;; * -halt-on-error: default.
;; * -interaction <mode>: like -halt-on-error, you can set the way
;;    the compilers behave on errors. Possible values for <mode> are
;;    'batchmode', 'errorstopmode', 'nonstopmode' and 'scrollmode'.
;; * -shell-escape: allow the use of \write18{<external command>}
;;    from within TeX documents. This is a potential security issue.
;; * -synctex=1: enable SyncTeX support.
;; You may use file local variable for convenience:
;;   % -*- tex-start-options: \"-shell-escape\"
;; Note that -shell-escape can also be toggled with universal
;; argument.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CUSTOM

(defcustom masterfile nil
  "The file that should be compiled. Useful for modular documents."
  :safe 'stringp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VARIABLES

(defvar tex-extension-list nil
  "List of known TeX exentsions. This list is used by `tex-clean'
  to purge all matching files.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTIONS
(defun tex-set-compiler ()
  "Set `compile-command' for TeX-based document."
  (hack-local-variables)
  (let (;; Master file.
        (local-master
         (if masterfile masterfile (if buffer-file-name buffer-file-name (error "Buffer has no file name")))))
    (set (make-local-variable 'compile-command)
         (concat tex-command
                 " " tex-start-options
                 " " tex-start-commands
                 " " (shell-quote-argument local-master)))))

(defun tex-clean ()
  "Remove all TeX temporary files. This command should be safe,
but there is no warranty."
  (interactive)
  (hack-local-variables)
  (let ((master (concat
                 (if masterfile
                     (file-name-sans-extension masterfile)
                   (file-name-sans-extension buffer-file-name))
                 ".")))
    (mapcar
     ;; Delete file if it exists.
     (lambda (argfile)
       (when (and (file-exists-p argfile) (file-writable-p argfile))
         (delete-file argfile)
         (message "[%s] deleted." argfile)))
     (mapcar
      ;; Concat file name with extensions.
      (lambda (arg) (concat master arg))
      tex-extension-list))))

(defun tex-pdf-compress ()
  "Use `masterfile' variable as default value for `pdf-compress'."
  (interactive)
  (require 'tool-pdf)
  (hack-local-variables)
  (let ((local-master (if masterfile masterfile buffer-file-name)))
    (pdf-compress local-master)))

(defun tex-pdf-view ()
  "Use `masterfile' variable as default value for `pdf-view'."
  (interactive)
  (require 'tool-pdf)
  (hack-local-variables)
  (let ((local-master (if masterfile masterfile buffer-file-name)))
    (pdf-view local-master)))

(defun tex-toggle-escape-char ()
  "Make backslash part of the word syntax or not.
This does not interfere with `subword-mode'."
  (interactive)
  (if (equal (char-syntax ?\\) ?\\)
      (progn
        (modify-syntax-entry ?\\ "w")
        (message "\\ is a not an escape character"))
    (modify-syntax-entry ?\\ "\\")
    (message "\\ is a an escape character")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TeX setup

(setq tex-start-options "-file-line-error-style -interaction=nonstopmode -synctex=1")
(setq tex-start-commands nil)

(add-hook-and-eval
 'tex-mode-hook
 (lambda ()
   (dolist (key '("\C-c\C-f" "\C-c\C-b"))
     (local-unset-key key))
   (set-face-attribute 'tex-verbatim nil :family "freemono")
   (set (make-local-variable 'compilation-scroll-output) t)
   (set (make-local-variable 'compilation-time-before-hide-window) 2)
   (set (make-local-variable 'paragraph-start) "
")
   ;; (set (make-local-variable 'use-hard-newlines) t)
   (local-set-key (kbd "<f9>") 'tex-pdf-view)
   (setq tex-command "pdftex")
   (add-hook 'compilation-before-hook 'tex-set-compiler nil t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Skeletons

(define-skeleton tex-array
  "Insert skel."
  "Format: "
  "$$\\begin{array}{" @ str "}" \n
  _ \n
  "\\end{array}$$" > \n)

(define-skeleton tex-bf "Insert skel." nil "{\\bf{" @ _ "}" @)
(define-skeleton tex-binom "Insert skel." nil "\\binom{" @ _ "}{" @ _ "}" @)
(define-skeleton tex-coprod "Insert skel." nil "\\coprod_{" @ _ "}^{" @ _ "}" @)
(define-skeleton tex-em "Insert skel." nil "{\\em{" @ _ "}" @)
(define-skeleton tex-footnote "Insert skel." nil "\\footnote{" @ _ "}" @)
(define-skeleton tex-href "Insert skel." "Desc: " "\\href{" @ _ "}{" @ str "}" @)
(define-skeleton tex-it "Insert skel." nil "{\\it{" @ _ "}" @)

(define-skeleton tex-main "Insert skel." nil
  "\\input eplain" \n
  @ > _ \n
    "\\bye" > @)

(define-skeleton tex-prod "Insert skel." nil "\\prod_{" @ _ "}^{" @ _ "}" @)
(define-skeleton tex-sum "Insert skel." nil "\\sum_{" @ _ "}^{" @ _ "}" @)
(define-skeleton tex-tt "Insert skel." nil "{\\tt{" @ _ "}" @)
(define-skeleton tex-url "Insert skel." nil "\\url{" @ _ "}" @)

(provide 'mode-tex)
