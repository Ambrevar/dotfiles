;;==============================================================================
;; Texinfo
;;==============================================================================

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CUSTOM

(defcustom masterfile nil
  "The file that should be compiled. Useful for modular documents."
  :safe 'stringp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VARIABLE

(defvar texinfo-viewer "zathura --fork"
  "PDF Viewer for Texinfo documents. You may want to fork the
viewer so that it detects when the same document is launched
twice, and persists when Emacs gets closed.")


(defvar texinfo-extension-list '("aux" "cp" "cps" "fn" "ky" "log" "pg" "toc" "tp" "vr" "vrs")
  "List of known Texinfo exentsions. This list is used by
  'texinfo-clean to purge all matching files.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTIONS

(defun texinfo-set-compiler ()
  "Use compile to process your Texinfo document."
  (interactive)
  (hack-local-variables)
  (let
      ;; Master file
      ( (local-master (if (not masterfile) buffer-file-name masterfile)))

    (set (make-local-variable 'compile-command)
         (concat "texi2pdf -b \"" local-master "\""))))

(defun texinfo-clean ()
  "Remove all Texinfo temporary files. This command should be safe,
but there is no warranty."
  (interactive)
  (hack-local-variables)
  (let
      ;; Master file.
      ((local-master (if (not masterfile) buffer-file-name masterfile)))

    (let
        ;; File name without extension.
        ((file
          (replace-regexp-in-string "texi" "" (file-name-nondirectory local-master))))

      ;; Concatate file name to list.
      (mapcar
       ;; Delete file if exist
       (lambda (argfile) (interactive)
         (when (and (file-exists-p argfile) (file-writable-p argfile))
           (delete-file argfile)
           (message "[%s] deleted." argfile)))
       (mapcar
        ;; Concat file name with extensions.
        (lambda (arg) (interactive) (concat file arg))
        texinfo-extension-list)))))

(defun texinfo-pdf-compress ()
  "Use `masterfile' variable as default value for `pdf-compress'."
  (interactive)
  (hack-local-variables)
  (let ((local-master (if (not masterfile) buffer-file-name masterfile)))
    (pdf-compress local-master)))

(defun texinfo-pdf-view ()
  "Call a PDF viewer for current buffer file. File name should be
properly escaped with double-quotes in case it has spaces."
  (interactive)
  (hack-local-variables)
  (let
      ;; Master file.
      ((local-master (if (not masterfile) buffer-file-name masterfile)))

    (shell-command
     (concat texinfo-viewer
             " \""
             (replace-regexp-in-string "\.texi$" "\.pdf" (file-name-nondirectory local-master))
             "\" &" ))
    (delete-windows-on "*Async Shell Command*")))

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
   (setq fill-column 80)
   (set (make-local-variable 'compilation-scroll-output) t)
   (set (make-local-variable 'compilation-hide-window) t)
   (set (make-local-variable 'use-hard-newlines) t)
   (local-set-key (kbd "C-c C-b") 'texinfo-menu-update)
   (local-set-key (kbd "<f9>") 'texinfo-pdf-view)
   (texinfo-set-compiler)))
