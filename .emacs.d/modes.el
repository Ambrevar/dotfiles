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

;; Let's use pdftex.
;; To use GnuPlot we need to allow external application to be called from TeX.
;; WARNING: the "--shell-escape" option is a potential security issue.
(setq tex-run-command "pdftex --shell-escape")
;; (setq tex-command "pdftex") ; Same as above ?
(setq latex-run-command "pdflatex --shell-escape")

;; TODO: display in TeX/LaTeX only.
(defun tex-pdf-view ()
  (interactive)
  (shell-command
   (concat "zathura --fork " 
           (replace-regexp-in-string "tex" "pdf" (file-name-nondirectory buffer-file-name))
           )
   )
  )

;; TODO: for  TeX/LaTeX only.
;; Replace tex-view
(define-key my-keys-minor-mode-map (kbd "C-c C-v") 'tex-pdf-view)
;; Save automatically before compiling.
(define-key my-keys-minor-mode-map (kbd "C-c C-f") (lambda () (interactive)
                                                     (save-buffer)
                                                     (tex-file)
                                                     ))

(defun tex-pdf-compress ()
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

(defun tex-clean ()
  (interactive)
  (defvar file-noext (replace-regexp-in-string ".tex" "" (file-name-nondirectory buffer-file-name)))
  (shell-command
   (concat "rm -f "
           file-noext
           ".aux "
           file-noext
           ".glg"
           file-noext
           ".glo"
           file-noext
           ".gls"
           file-noext
           ".idx "
           file-noext
           ".ilg "
           file-noext
           ".ind "
           file-noext
           ".lof "
           file-noext
           ".log "
           file-noext
           ".nav "
           file-noext
           ".out "
           file-noext
           ".snm "
           file-noext
           ".tns "
           file-noext
           ".toc "
           file-noext
           ".xdy"
           )
   )
  )

;; ;; Add '--shell-escape' switch to compilation command (useful for using GnuPlot from TikZ)
;; (eval-after-load "tex"
;;   '(setcdr (assoc "LaTeX" TeX-command-list)
;; 	   '("%`%l%(mode) --shell-escape %' %t"
;; 	    TeX-run-TeX nil (latex-mode doctex-mode) :help "Run LaTeX")
;; 	  )
;;   )

;; ;; Theme
;; (defun my-tex-font-hook ()
;;   (set-face-foreground 'font-latex-sedate-face "brightred" )
;;   (set-face-bold-p 'font-latex-sedate-face t)
;; )
;; (add-hook 'TeX-mode-hook 'my-tex-font-hook)

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
(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'c++-mode-hook 'my-c-mode-hook)
(add-hook 'cpp-mode-hook 'my-c-mode-hook)

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
                             file))))))

;;==============================================================================
;; Common LISP
;;==============================================================================
(setq inferior-lisp-program "clisp")

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
         (line-err-info-list  (nth 0 (flymake-find-err-info flymake-err-info 
line-no)))
         (menu-data           (flymake-make-err-menu-data line-no 
line-err-info-list)))
    (if menu-data
        (let ((messages))
          (push (concat (car menu-data) ":") messages)
          (dolist (error-or-warning (cadr menu-data))
            (push (car error-or-warning) messages))
          (message "%s" (mapconcat #'identity (reverse messages) "\n"))))))

(define-key my-keys-minor-mode-map (kbd "<f10>")
  'flymake-display-err-message-for-current-line)
