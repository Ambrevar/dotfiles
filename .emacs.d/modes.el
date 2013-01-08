;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;==============================================================================
;; Automode (Mode recognition)
;;==============================================================================

;; rc files support
(setq auto-mode-alist
      (append
       '(("rc\\'" . sh-mode)
         )
       auto-mode-alist)
      )

;; Shell support
;; We do not put 'sh' only because it could get messy. Emacs knows it anyway.
(setq auto-mode-alist
      (append
       '(("\\(bash\\'\\|zsh\\'\\|csh\\'\\|tcsh\\'\\|ksh\\'\\)" . sh-mode)
         )
       auto-mode-alist)
      )

;; Read Matlab files in Octave mode.
(setq auto-mode-alist
      (append
       '(("\\.m\\'" . octave-mode)
         )
       auto-mode-alist)
      )

;; Read pl files in prolog mode.
;; WARNING: this extension is shared with Perl.
(setq auto-mode-alist
      (append
       '(("\\.pl\\'" . prolog-mode)
         )
       auto-mode-alist)
      )

;; Mutt support.
(setq auto-mode-alist
      (append
       '(("/tmp/mutt.*" . mail-mode)
         )
       auto-mode-alist)
      )

;; Arch Linux PKGBUILD
(setq auto-mode-alist
      (append
       '(("PKGBUILD" . sh-mode)
         )
       auto-mode-alist)
      )

;; README
(setq auto-mode-alist
      (append
       '(("README" . text-mode)
         )
       auto-mode-alist)
      )

;;==============================================================================
;; Auto-Insert
;;==============================================================================

;; autoinsert C/C++ header
(define-auto-insert
  (cons "\\.\\([Hh]\\|hh\\|hpp\\)\\'" "My C / C++ header")
  '(nil
    "/" (make-string 79 ?*) "\n"
    " * @file " (file-name-nondirectory buffer-file-name) "\n"
    " * @date \n"
    " * @brief \n"
    " *\n"
    " " (make-string 78 ?*) "/\n\n"
    (let* ((noext (substring buffer-file-name 0 (match-beginning 0)))
           (nopath (file-name-nondirectory noext))
           (ident (concat (upcase nopath) "_H")))
      (concat "#ifndef " ident "\n"
              "#define " ident  " 1\n\n\n"
              "\n\n#endif // " ident "\n"))
    ))

;; auto insert C/C++
(define-auto-insert
  (cons "\\.\\([Cc]\\|cc\\|cpp\\)\\'" "My C++ implementation")
  '(nil
    "/" (make-string 79 ?*) "\n"
    " * @file " (file-name-nondirectory buffer-file-name) "\n"
    " * @date \n"
    " * @brief \n"
    " *\n"
    " " (make-string 78 ?*) "/\n\n"
    (let* ((noext (substring buffer-file-name 0 (match-beginning 0)))
           (nopath (file-name-nondirectory noext))
           (ident (concat nopath ".h")))
      (if (file-exists-p ident)
          (concat "#include \"" ident "\"\n")))
    ))

;; auto insert LaTeX Article
(define-auto-insert
  (cons "\\.\\(tex\\)\\'" "My LaTeX implementation")
  '(nil
    (make-string 80 ?%) "\n"
    "\\documentclass[11pt]{article}\n"
    "\\usepackage[utf8]{inputenc}\n"
    "\\usepackage[T1]{fontenc}\n"
    "% \\usepackage{lmodern}\n"
    (make-string 80 ?%) "\n"

    "\\title{Title}\n"
    "\\author{\\textsc{P.~Neidhardt}}\n"
    ))

;;==============================================================================
;; TeX and LaTeX
;;==============================================================================

;; I find the default tex-mode and AucTeX quiet disappointing. I'm using custom
;; functions for everything.

(defvar tex-my-compiler "pdftex"
  "This is the name of the executable called upon TeX compilations.
Examples: pdftex, pdflatex, xetex, xelatex, luatex, lualatex...")

(defvar tex-my-compiler-options "-file-line-error-style -interaction nonstopmode"
  "The options to the tex compiler. Options are set between the
compiler name and the file name.

Interresting options:

* -file-line-error-style: change the style of error report to
   display file name and line first.

* -halt-on-error: default.

* -interaction <mode>: like -halt-on-error, you can set the way
   the compilers behave on errors. Possible values for <mode> are
   'batchmode', 'errorstopmode', 'nonstopmode' and 'scrollmode'.

* -shell-escape: allow the use of \write18{<external command>}
   from within TeX documents. This is a potential security issue.

You may use file local variable for convenience:

% -*- tex-my-compiler-options: \"-shell-escape\"

Note that -shell-escape can also be toggled with universal
argument.")

(defvar tex-my-startcommands ""
  "You can call a TeX compiler upon a string instead of a file.
This is actually useful if you want to customize your
compilation.

If this variable is not an empty string, the mandatory \" is
prepended and \\input\" is appended, so that the target file gets
read; otherwise the TeX compiler would stop there.

You may use it to act on the process, like the default behaviour:
  \\nonstopmode
which will continue the process whenever an error is
encountered. There is an command-line argument for that on most
compilers, that is is rarely useful.

If you use a color theme, or any conditional variable inside your
document, you may define it here:
  \\def\\myvar{mycontent}

A pratical way to use this feature is to define a file local
variable, e.g. on the first line:
  % -*- tex-my-startcommands: \"\\def\\locale{en}\" -*-
")

(defun tex-my-compile ()
  "Use compile to process your TeX-based document. Use a prefix
  argument to call the compiler along the '-shell-escape'
  option. This will enable the use of '\write18{<external
  command>}' from within TeX documents, which need to
  allow external application to be called from TeX.

This may be useful for some features like GnuPlot support with TikZ.

WARNING: the -shell-escape option is a potential security issue."

  (interactive)
  (cond
   ((string= "latex-mode" major-mode) (setq tex-my-compiler "pdflatex"))
   ((string= "plain-tex-mode" major-mode) (setq tex-my-compiler "pdftex"))
   (t (setq tex-my-compiler "pdftex") (message "Warning: unknown major mode. Trying pdftex."))
   )

  ;; If tex-my-startcommands has some content, we make sure it is a string that loads the file.
  (if (not (string= "" tex-my-startcommands))
      (setq tex-my-startcommands (concat "\"" tex-my-startcommands "\\input\"")))

  ;; Support of prefix argument to toggle -shell-escape.
  (if (equal current-prefix-arg '(4)) (setq tex-my-shell-escape "-shell-escape")
    (setq tex-my-shell-escape ""))

  (setq tex-my-compile-command (concat tex-my-compiler " "  tex-my-shell-escape " " tex-my-compiler-options " " tex-my-startcommands " " buffer-file-name))
  ;; (message tex-my-compile-command) ;; Debug only.
  (save-buffer)
  (setq compilation-scroll-output t)
  (compile tex-my-compile-command)

  ;; If no user interaction for 2 seconds, hide the compilation window.
  (sit-for 2)
  (delete-windows-on "*compilation*")
  )

;; TODO: rewrite this function using lists and/or macros.
(defun tex-clean ()
  "Remove all TeX temporary files. This command should be safe,
but there is no warranty."
  (interactive)
  (defvar file-noext (replace-regexp-in-string ".tex" "" (file-name-nondirectory buffer-file-name)))
  (shell-command
   (concat "rm -f "
           file-noext ".aux " file-noext ".glg" file-noext ".glo" file-noext ".gls" file-noext ".idx " file-noext ".ilg " file-noext ".ind " file-noext ".lof " file-noext ".log " file-noext ".nav " file-noext ".out " file-noext ".snm " file-noext ".tns " file-noext ".toc " file-noext ".xdy"
           )
   )
  )

(defun tex-pdf-compress ()
  "PDF compressions might really strip down the PDF size. The
compression depends on the fonts used. Do not use this command if
your document embeds raster graphics."
  (interactive)
  (defvar file-noext (replace-regexp-in-string ".tex" "" (file-name-nondirectory buffer-file-name)))
  (defvar file (replace-regexp-in-string "tex" "pdf" (file-name-nondirectory buffer-file-name)))
  (shell-command
   (concat "if [ -e "
           file
           " ]; then gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile="
           file-noext
           "-COMPRESSED.pdf "
           file
           " && rm -rf "
           file
           " && mv "
           file-noext
           "-COMPRESSED.pdf "
           file
           " ; fi"
           )
   )
  )

(defun tex-pdf-view ()
  "Call a PDF viewer for current buffer file."
  (interactive)
  (shell-command
   (concat "zathura --fork " 
           (replace-regexp-in-string "\.tex$" "\.pdf &" (file-name-nondirectory buffer-file-name))
           )
   )
  (delete-windows-on "*Async Shell Command*")
  )

(add-hook
 'tex-mode-hook
 (lambda ()
   (dolist (key '("\C-c\C-f" "\C-c\C-b"))
     (local-unset-key key))   
   (local-set-key (kbd "C-c C-c") 'tex-my-compile)
   (local-set-key (kbd "C-c C-v") 'tex-pdf-view)
   )
 )

;;==============================================================================
;; HTML
;;==============================================================================

(add-hook 'html-mode-hook
          (lambda ()
            (turn-off-auto-fill)
            (toggle-truncate-lines)
))

;;==============================================================================
;; C-mode
;;==============================================================================
(require 'compile)
(add-hook 'c-mode-hook
          (lambda ()
            (unless (file-exists-p "Makefile")
              (set (make-local-variable 'compile-command)
                   ;; emulate make's .c.o implicit pattern rule, but with
                   ;; different defaults for the CC, CPPFLAGS, and CFLAGS
                   ;; variables:
                   ;; $(CC) -c -o $@ $(CPPFLAGS) $(CFLAGS) $<
                   (let ((file (file-name-nondirectory buffer-file-name)))
                     (format "%s -o %s %s %s %s"
                             (or (getenv "CC") "gcc")
                             (file-name-sans-extension file)
                             (or (getenv "CPPFLAGS") "-DDEBUG=9")
                             (or (getenv "CFLAGS") "-ansi -pedantic -std=c99 -Wall -Wextra -Wshadow -lm -g3 -O0")
                             file))))

            (local-set-key (kbd "C-c C-f") 'compile)
            ;; (define-key my-keys-minor-mode-map (kbd "<f12>") 'next-error)
            (local-set-key (kbd "M-a") 'beginning-of-defun)
            (local-set-key (kbd "M-e") 'end-of-defun)

            )
          )

;;==============================================================================
;; Common LISP
;;==============================================================================
(setq inferior-lisp-program "clisp")

;;==============================================================================
;; Python
;;==============================================================================

(add-hook
 'python-mode-hook
 (lambda ()
   (defun python-my-compile ()
     "Use compile to run python programs."
     (interactive)
     (compile (concat "python " buffer-name))
     )
   (setq compilation-scroll-output t)
   (local-set-key "\C-c\C-c" 'python-my-compile) 
   )
 )

;;==============================================================================
;; Flymake
;;==============================================================================

;; Flymake has a bug that prevents menu from spawning in a console. We redefine
;; the function to spawn the error message in the mini-buffer.

(defun flymake-display-err-message-for-current-line ()
  "Display a message with errors/warnings for current line if it
has errors and/or warnings."
  (interactive)
  (let* ((line-no             (flymake-current-line-no))
         (line-err-info-list  (nth 0 (flymake-find-err-info flymake-err-info line-no)))
         (menu-data           (flymake-make-err-menu-data line-no line-err-info-list)))
    (if menu-data
        (let ((messages))
          (push (concat (car menu-data) ":") messages)
          (dolist (error-or-warning (cadr menu-data))
            (push (car error-or-warning) messages))
          (message "%s" (mapconcat #'identity (reverse messages) "\n"))))))

(define-key my-keys-minor-mode-map (kbd "<f10>")
  'flymake-display-err-message-for-current-line)

;;==============================================================================
;; GUD
;;==============================================================================

;; Set GDB to display many windows by default.
(setq gdb-many-windows t)

