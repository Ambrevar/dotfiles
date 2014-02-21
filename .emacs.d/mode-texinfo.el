;;==============================================================================
;; Texinfo using TeX setup
;;==============================================================================
(require 'mode-tex)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VARIABLE

(defvar tex-default-compiler "texi2pdf -b"
  "Default compiler for LaTeX mode. Used if `tex-compiler' is
empty.")

(defvar tex-extension-list '("aux" "cp" "cps" "fn" "ky" "log" "pg" "toc" "tp" "vr" "vrs")
  "List of known Texinfo exentsions. This list is used by
  'texinfo-clean to purge all matching files.")

(defcustom tex-compiler-options ""
  "The options to the tex compiler. Options are set between the
compiler name and the file name.

Interesting options:

* --shell-escape: allow the use of \write18{<external command>}
   from within TeX documents. This is a potential security issue.

You may use file local variable for convenience:

% -*- tex-compiler-options: \"--shell-escape\"

Note that --shell-escape can also be toggled with universal
argument."
  :safe 'stringp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTIONS

(defun texinfo-menu-update ()
  "Update texinfo node menu automatically."
  (interactive)
  (hack-local-variables)
  (let
      ;; Master file.
      ((local-master (if (not masterfile) buffer-file-name masterfile)))

    (texinfo-multiple-files-update local-master t 8)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HOOKS

(add-hook
 'texinfo-mode-hook
 (lambda ()
   (setq fill-column 80) ;; Really needed?
   (local-set-key (kbd "C-c C-b") 'texinfo-menu-update)))
