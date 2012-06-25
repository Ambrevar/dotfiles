;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs config
;; 2012-06-22
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
(setq linum-format "%d ")
(global-linum-mode 1)

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
(add-hook 'c-mode-common-hook 
          (lambda ()
            (auto-fill-mode 1)
            (set (make-local-variable 'fill-nobreak-predicate)
                 (lambda ()
                   (not (eq (get-text-property (point) 'face)
                            'font-lock-comment-face ))
                   ))
            ))


;; Man-mode
(setenv "MANWIDTH" "80")

;;==============================================================================
;; Theme
;;==============================================================================
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(minibuffer-prompt ((t (:foreground "cyan")))))

;;==============================================================================
;; Completion
;;==============================================================================
;(global-set-key (kbd "C-<tab>") 'dabbrev-expand)
;(define-key minibuffer-local-map (kbd "C-<tab>") 'dabbrev-expand)



;;==============================================================================
;; Example of automode default modification
;;==============================================================================
(setq auto-mode-alist
      (append
       '(("\\.m$" . octave-mode)
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

(global-set-key (kbd "M-<next>") 'select-next-window)
(global-set-key (kbd "M-<prior>")  'select-previous-window)
;; (global-set-key (kbd "M-<up>") 'select-next-window)
;; (global-set-key (kbd "M-<down>") 'select-previous-window)
(global-set-key (kbd "M-<left>") 'previous-buffer)
(global-set-key (kbd "M-<right>") 'next-buffer)


;;==============================================================================
;; Comment DWIM -- toggle comment line
;;==============================================================================

;; Original idea from
;; http://www.opensubscriber.com/message/emacs-devel@gnu.org/10971693.html
(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
        If no region is selected and current line is not blank and we are not at the end of the line,
        then comment current line.
        Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))
(global-set-key "\M-;" 'comment-dwim-line)



;;==============================================================================
;; My Keys Minor-Mode
;;==============================================================================

(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")

;; Duplicate line
(define-key my-keys-minor-mode-map (kbd "C-d") 'duplicate-line)

;; Paste from clipboard.
;; (define-key my-keys-minor-mode-map (kbd "M-p") (kbd "C-u M-! xclip <SPC> -o"))
(define-key my-keys-minor-mode-map (kbd "M-p") (kbd "C-u M-! xsel <SPC> -o"))

;; Compilation
(define-key my-keys-minor-mode-map (kbd "<f10>") 'compile)
(define-key my-keys-minor-mode-map (kbd "<f12>") 'next-error)

;; Window resize
(define-key my-keys-minor-mode-map (kbd "S-C-<right>") 'shrink-window-horizontally)
(define-key my-keys-minor-mode-map (kbd "S-C-<left>") 'enlarge-window-horizontally)
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

;; (add-to-list 'load-path "/usr/share/emacs/site-lisp/yas")
;; (require 'yasnippet) ;; not yasnippet-bundle
;; (yas/initialize)

;; ;; Develop and keep personal snippets under ~/emacs.d/mysnippets
;; ;; (setq yas/root-directory "~/emacs.d/mysnippets")
;; (setq yas/root-directory "~/.emacs.d/plugins/yas/snippets" )

;; ;; "/usr/share/emacs/site-lisp/yas/snippets"

;; ;; Load the snippets
;; (yas/load-directory "/usr/share/emacs/site-lisp/yas/snippets")
;; (yas/load-directory yas/root-directory)

;;==============================================================================
;; Auto-Complete
;;==============================================================================
(add-to-list 'load-path "/usr/share/emacs/site-lisp/auto-complete")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "/usr/share/emacs/site-lisp/auto-complete/ac-dict")
(ac-config-default)

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

;; (setq mediawiki-site-alist
;;       (append '("ArchWiki" "https://wiki.archlinux.org/" "username" "password" "Main Page")
;;               mediawiki-site-alist))
