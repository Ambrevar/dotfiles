;; TeX

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

(dolist (key '("\C-c\C-f" "\C-c\C-b"))
  (local-unset-key key))
(local-set-key (kbd "<f9>") 'tex-pdf-view)
(local-set-key (kbd "<f10>") 'tex-compile)

(defvar-local tex-masterfile nil
  "The file that should be compiled. Useful for modular documents.")

(defcustom tex-compilation-delay 2
  "Seconds before hiding the compilation window."
  :type 'number
  :group 'tex)

(defcustom tex-extension-list nil
  "List of known TeX exentsions. This list is used by `tex-clean'
  to purge all matching files."
  :type '(repeat list)
  :group 'tex)

(defcustom tex-index-command "makeindex"
  "The TeX index file generator."
  :type 'string
  :group 'tex)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions

(defun tex-set-compiler ()
  "Set `compile-command' for TeX-based document."
  (interactive)
  (hack-local-variables)
  (let* (;; Master file.
         (local-master (or tex-masterfile buffer-file-name (error "Buffer has no file name")))
         (dirname (file-name-directory local-master))
         (basename (file-name-sans-extension (file-name-nondirectory local-master)))
         ;; Note: makeindex fails with absolute file names, we need relative names.
         (idxfile ))
    (setq compile-command
          (concat
           "cd " (if dirname (shell-quote-argument dirname) ".") " && "
           (when (executable-find tex-index-command)
             (concat tex-index-command " " (shell-quote-argument (concat basename ".idx")) "; "))
           (when (executable-find tex-bibtex-command)
             (concat tex-bibtex-command " " (shell-quote-argument basename) "; "))
           tex-command
           " " tex-start-options
           " " tex-start-commands
           " " (shell-quote-argument basename)))))

(defun tex-clean ()
  "Remove all TeX temporary files.
This command should be safe, but there is no warranty."
  (interactive)
  (hack-local-variables)
  (let ((master (file-name-sans-extension (or tex-masterfile buffer-file-name))))
    (dolist (file (mapcar
                   ;; Concat file name with extensions.
                   (lambda (arg) (concat master "." arg))
                   tex-extension-list))
      ;; Delete file if it exists.
      (when (and (file-exists-p file) (file-writable-p file))
        (delete-file file)
        (message "Deleted %S." file)))))

(defun tex-pdf-compress ()
  "Use `tex-masterfile' variable as default value for `pdf-compress'."
  (interactive)
  (require 'tool-pdf)
  (hack-local-variables)
  (let ((local-master (or tex-masterfile buffer-file-name)))
    (pdf-compress local-master)))

(defun tex-pdf-view ()
  "Use `tex-masterfile' variable as default value for `pdf-view'."
  (interactive)
  (require 'tool-pdf)
  (hack-local-variables)
  (let ((local-master (or tex-masterfile buffer-file-name)))
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

(defun tex-compile ()
  "Compile the TeX document."
  (interactive)
  (compile compile-command)
  (sit-for tex-compilation-delay)
  (delete-windows-on "*compilation*"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TeX setup

(setq-default tex-command "pdftex")
(setq tex-command "pdftex")
(setq-default tex-start-options "-file-line-error-style -interaction=nonstopmode -synctex=1")
;; Use the following variable to append file local commands without erasing
;; default options.
(setq-default tex-start-commands nil)

;; `tex-mode' sets `indent-tabs-mode' to nil, invoking the following
;; argument: "TABs in verbatim environments don't do what you think." Not
;; sure how relevant this bad comment is. We revert it.
(add-hook-and-eval 'tex-mode-hook 'turn-on-indent-tabs)
(add-hook-and-eval 'tex-mode-hook 'turn-on-newline-paragraph)
(add-hook-and-eval 'tex-mode-hook 'turn-on-newline-paragraph)
(add-hook-and-eval 'tex-mode-hook 'tex-set-compiler)

;; Not sure how useful that is:
;; (set (make-local-variable 'use-hard-newlines) t)

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
