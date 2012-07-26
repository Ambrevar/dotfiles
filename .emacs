;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs config
;; 2012-07-18
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;==============================================================================
;; General
;;==============================================================================

;; Make questions less annoying
(defalias 'yes-or-no-p 'y-or-n-p)

;; Print column number
(column-number-mode 1)

;; Kill whole line
(setq kill-whole-line t)

;; Line numbers
(autoload 'linum-mode "linum" "toggle line numbers on/off" t)
(global-set-key (kbd "C-<f5>") 'linum-mode)
;; (setq linum-format "%-4d ")
(setq linum-format "%d ")
(global-linum-mode 1) ;; FIXME: This may generate warnings. Bug?

;; Indentation
;(setq standard-indent 4) ;; Set standard indent to 2 rather that 4
(setq-default tab-width 4) ; Tab width set to 2 spaces
(setq-default indent-tabs-mode nil) ; Indentation cannot insert tabs
(setq c-default-style "linux" c-basic-offset 4) ;; Identation style

;; Line by line scrolling
(setq scroll-step 1)

;; Highlight selections -- not activated by default on old emacs.
(transient-mark-mode 1)

;; Mousewheel scrolling -- Does not work?
;(mouse-wheel-mode t)

;; No trailing whitespace
;; WARNING: this can break some configuration files needing whitespaces at the
;; end.
;; (add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Abbreviation support
(setq default-abbrev-mode t)

;; Remember last cursor position
(setq save-place-file "~/.emacs.d/.saveplace")
(setq-default save-place t)
(require 'saveplace)

;; Disable autosave features
(setq auto-save-default nil)

;; Place Backup Files in Specific Directory
(setq backup-inhibited t) ;; Disable backup files.
;(setq make-backup-files t) ;; Enable backup files.
;(setq version-control t) ;; Enable versioning with default values (keep five last versions, I think!)
;(setq backup-directory-alist (quote ((".*" . "~/.emacs.d/.backups/")))) ;; Save all backup file in this directory.

;; Remove menu-bar
(menu-bar-mode -1)

;; Set Fill Column
(setq-default fill-column 80)
;; (setq auto-fill-mode 1) ;; Does not work ?
(add-hook 'text-mode-hook 'turn-on-auto-fill)
;; (add-hook 'c-mode-common-hook 
;;           (lambda ()
;;             (auto-fill-mode 1)
;;             (set (make-local-variable 'fill-nobreak-predicate)
;;                  (lambda ()
;;                    (not (eq (get-text-property (point) 'face)
;;                             'font-lock-comment-face ))
;;                    ))
;;             ))


;; Man-mode
(setenv "MANWIDTH" "80")

;; Browser
(setq browse-url-generic-program (executable-find "luakit")
browse-url-browser-function 'browse-url-generic)

;;==============================================================================
;; Theme
;;==============================================================================

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

(mapcar
 (lambda (mode)
   (font-lock-add-keywords
    mode
    '(
      ("[^[:digit:][:space:]][[:space:]]*\\(-\\)[[:digit:]]+" 1 font-lock-constant-face)
      ("\\(0x[[:digit:]a-fA-F]+\\)[^[:alnum:]_]" 1 font-lock-constant-face)
      ("[^[:alnum:]_]\\([[:digit:]]*\\.?[[:digit:]]+\\)[^[:alnum:]_.]" 1 font-lock-constant-face)
      ("\\<\\(FIXME\\):" 1 font-lock-warning-face prepend)
      ("\\<\\(TODO\\):" 1 font-lock-warning-face prepend)
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

;; (set-face-background 'lazy-highlight  "brightgreen" ) 
;; (set-face-background 'secondary-selection "SkyBlue4")
;; (set-face-background 'trailing-whitespace "red1")
;; (set-face-background 'vertical-border  "color-17")
;; (set-face-foreground 'warning  "DarkOrange")
;; (set-face-bold-p 'warning  t)
;; (set-face-foreground 'nobreak-space "cyan")
;; (set-face-foreground 'success "Green1")
;; (set-face-bold-p 'success t)

;;==============================================================================
;; Completion
;;==============================================================================
;(global-set-key (kbd "C-<tab>") 'dabbrev-expand)
;(define-key minibuffer-local-map (kbd "C-<tab>") 'dabbrev-expand)

;;==============================================================================
;; Automode default modification
;;==============================================================================

;; rc support
(setq auto-mode-alist
      (append
       '(("rc\\'" . sh-mode)
         )
       auto-mode-alist)
      )

;; Shell support
;; We do not put 'sh' only because it could get messy.
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;==============================================================================
;; Unfill paragraph
;;==============================================================================

(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun unfill-region ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-region (region-beginning) (region-end) nil)))

;;==============================================================================
;; Duplicate line
;;==============================================================================
(defun duplicate-line (arg)
  "Duplicate current line, leaving point in lower line."
  (interactive "*p")

  ;; save the point for undo
  (setq buffer-undo-list (cons (point) buffer-undo-list))

  ;; local variables for start and end of line
  (let ((bol (save-excursion (beginning-of-line) (point)))
        eol)
    (save-excursion

      ;; don't use forward-line for this, because you would have
      ;; to check whether you are at the end of the buffer
      (end-of-line)
      (setq eol (point))

      ;; store the line and disable the recording of undo information
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t)
            (count arg))
        ;; insert the line arg times
        (while (> count 0)
          (newline)         ;; because there is no newline in 'line'
          (insert line)
          (setq count (1- count)))
        )

      ;; create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list)))
    ) ; end-of-let

  ;; put the point in the lowest line and return
  (next-line arg))


;;==============================================================================
;; Select windows
;;==============================================================================

(defun select-next-window ()
  "Switch to the next window"
  (interactive)
  (select-window (next-window)))

(defun select-previous-window ()
  "Switch to the previous window"
  (interactive)
  (select-window (previous-window)))

(global-set-key (kbd "C-<next>") 'select-next-window)
(global-set-key (kbd "C-<prior>")  'select-previous-window)
;; (global-set-key (kbd "M-<up>") 'previous-buffer)
;; (global-set-key (kbd "M-<down>") 'next-buffer)
;; (global-set-key [?\e <left>] 'previous-buffer)
;; (global-set-key (kbd "C-M-<right>") 'next-buffer)


;;==============================================================================
;; Comment DWIM -- toggle comment line
;;==============================================================================

;; Original idea from
;; http://www.opensubscriber.com/message/emacs-devel@gnu.org/10971693.html
(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command. If no region is
selected and current line is not blank and we are not at the end
of the line, then comment current line.  Replaces default
behaviour of comment-dwim, when it inserts comment at the end of
the line."
  (interactive "*P")
  (comment-normalize-vars)
  ;; (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
  (if (and (not (region-active-p)) )
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

(global-set-key "\M-;" 'comment-dwim-line)

;;==============================================================================
;; My Keys Minor-Mode
;;==============================================================================

(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")

;; Duplicate line
(define-key my-keys-minor-mode-map (kbd "C-d") 'duplicate-line)
(define-key my-keys-minor-mode-map (kbd "M-a") 'beginning-of-defun)
(define-key my-keys-minor-mode-map (kbd "M-e") 'end-of-defun)

;; Paste from clipboard.
;; (define-key my-keys-minor-mode-map (kbd "M-p") (kbd "C-u M-! xclip <SPC> -o"))
(define-key my-keys-minor-mode-map (kbd "C-<f6>") (kbd "M-| xsel <SPC> -p <SPC> -i"))
(define-key my-keys-minor-mode-map (kbd "C-<f7>") (kbd "C-u M-! xsel <SPC> -o"))
(define-key my-keys-minor-mode-map (kbd "C-<f8>") (kbd "C-u M-! xsel <SPC> -o -b"))

;; Modern scrolling
(global-set-key [next]
                (lambda () (interactive)
                  (condition-case nil (scroll-up)
                    (end-of-buffer (goto-char (point-max))))))

(global-set-key [prior]
                (lambda () (interactive)
                  (condition-case nil (scroll-down)
                    (beginning-of-buffer (goto-char (point-min))))))

;; Compilation
(define-key my-keys-minor-mode-map (kbd "<f10>") 'compile)
(define-key my-keys-minor-mode-map (kbd "<f12>") 'next-error)

;; Window resize
;; (define-key my-keys-minor-mode-map (kbd "S-C-<right>") 'shrink-window-horizontally)
;; (define-key my-keys-minor-mode-map (kbd "S-C-<left>") 'enlarge-window-horizontally)
;; (define-key my-keys-minor-mode-map (kbd "S-C-<down>") 'shrink-window)
;; (define-key my-keys-minor-mode-map (kbd "S-C-<up>") 'enlarge-window)


;; My-Keys options
(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " my-keys" 'my-keys-minor-mode-map)

(my-keys-minor-mode 1)

(defun my-minibuffer-setup-hook ()
  (my-keys-minor-mode 0))

(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)

;;==============================================================================
;; Programming
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Plugins
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;==============================================================================
;; smart-compile
;;==============================================================================

(add-to-list 'load-path "~/.emacs.d/plugins/smart-compile")

(require 'smart-compile)

(defun my-c-mode-hook ()
  (local-set-key (kbd "<f9>") (kbd "C-x C-s M-x smart-compile C-j C-j"))
  )
(define-key my-keys-minor-mode-map (kbd "C-<f9>") 'next-error)

(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'c++-mode-hook 'my-c-mode-hook)
(add-hook 'cpp-mode-hook 'my-c-mode-hook)


;;==============================================================================
;; Yasnippet
;;==============================================================================
;; Yasnippet is slow when loading snippets from source.
;; Generate a bundle instead: yas/compile-bundle
;; Besides you can convert the generated file to bytecode.

;; (add-to-list 'load-path "~/.emacs.d/plugins")
;; (require 'yasnippet-bundle)

;; Next follows a traditional, yet not-optimized configuration for Yasnippet.
(add-to-list 'load-path "/usr/share/emacs/site-lisp/yas")
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)

;; Personal snippets
(setq yas/root-directory "~/.emacs.d/plugins/yas/snippets" )

;; Load the snippets
; (yas/load-directory  "~/.emacs.d/plugins/yas/snippets") ; Warning: slow!
(yas/load-directory yas/root-directory)

;;==============================================================================
;; Auto-Complete
;;==============================================================================
;; (add-to-list 'load-path "/usr/share/emacs/site-lisp/auto-complete")
;; (require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories "/usr/share/emacs/site-lisp/auto-complete/ac-dict")
;; (ac-config-default)

;;==============================================================================
;; AucTeX
;;==============================================================================

;; Activate AucTeX
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

;; Compile to PDF by default
(setq TeX-PDF-mode t)

(setq TeX-view-program-selection
      '((output-dvi "DVI Viewer")
        (output-ps "PS Viewer")
        (output-pdf "PDF Viewer")
        (output-html "Web browser")))
(setq TeX-view-program-list
      '(("DVI Viewer" "zathura --fork %o")
        ("PS Viewer" "zathura --fork %o")
        ("PDF Viewer" "zathura --fork %o")
        ("Web browser" "luakit %o")))

;; Add 'Compress PDF' compilation command
(eval-after-load "TeX"
  '(add-to-list 'TeX-command-list
                '("Compress" "if [ -e %s.pdf ]; then gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=%s-COMPRESSED.pdf %s.pdf && rm -rf %s.pdf && mv %s-COMPRESSED.pdf %s.pdf;fi" TeX-run-command nil t :help "Compress PDF"
                  )
                t )
  )

;; Add '--shell-escape' switch to compilation command (useful for using GnuPlot from TikZ)
(eval-after-load "tex"
  '(setcdr (assoc "LaTeX" TeX-command-list)
	   '("%`%l%(mode) --shell-escape %' %t"
	    TeX-run-TeX nil (latex-mode doctex-mode) :help "Run LaTeX")
	  )
  )

(defun my-tex-mode-hook ()
(local-set-key (kbd "<f9>") (kbd "C-x C-s C-c C-c C-j")))

(add-hook 'TeX-mode-hook 'my-tex-mode-hook)


;; Theme
(defun my-tex-font-hook ()
  (set-face-foreground 'font-latex-sedate-face "brightred" ) 
  (set-face-bold-p 'font-latex-sedate-face t) 
)
(add-hook 'TeX-mode-hook 'my-tex-font-hook)


;;==============================================================================
;; Lua
;;==============================================================================
(setq auto-mode-alist (cons '("\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)

;;==============================================================================
;; DoxyMacs
;;==============================================================================

;; (require 'doxymacs)
;; (add-hook 'c-mode-common-hook 'doxymacs-mode)

;; ;; Fontified comments.
;; (defun my-doxymacs-font-lock-hook ()
;;   (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
;;       (doxymacs-font-lock)))
;; (add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)

;;==============================================================================
;; MediaWiki
;;==============================================================================

(require 'mediawiki)

;; TODO:  make it 'customize' independant.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mediawiki-site-alist (quote (("Wikipedia" "http://en.wikipedia.org/w/" "Ambrevar" "" "Main Page") ("Wikibooks" "http://en.wikibooks.org/w/" "Ambrevar" "" "LaTeX") ("ArchLinux" "https://wiki.archlinux.org/" "Ambrevar" "" "Mutt")))))


(setq mediawiki-mode-hook (lambda ()
                            (visual-line-mode 1)
                            (turn-off-auto-fill)
                            (define-key mediawiki-mode-map (kbd "C-c RET") 'mediawiki-open-page-at-point)
                            (define-key mediawiki-mode-map (kbd "C-c o") 'mediawiki-browse)
))


;;=============================================================================↑=
;;==============================================================================
;;
