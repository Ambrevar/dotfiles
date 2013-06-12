;;==============================================================================
;; Texinfo
;;==============================================================================

(defcustom texinfo-my-viewer "zathura --fork"
  "PDF Viewer for Texinfo documents. You may want to fork the viewer
so that it detects when the same document is launched twice, and
persists when Emacs gets closed."
  :safe 'stringp)

(defcustom texinfo-my-masterfile nil
  "[Local variable]

The file that should be compiled."
  :safe 'stringp)

(defun texinfo-my-compile ()
  "Use compile to process your Texinfo document."
  (interactive)
  (let
      ;; Master file
      ( (local-master
         (if (not texinfo-my-masterfile)
             buffer-file-name
           texinfo-my-masterfile)))

    (let
        ;; Final command
        ( (local-compile-command
           (concat  "texi2pdf -b \"" local-master "\"")))

      (message local-compile-command) ;; Debug only.
      (save-buffer)
      (setq compilation-scroll-output t)
      (compile local-compile-command)

      ;; If no user interaction for 2 seconds, hide the compilation window.
      (sit-for 2)
      (delete-windows-on "*compilation*"))))


(defcustom texinfo-my-extension-list '("aux" "cp" "cps" "fn" "ky" "log" "pg" "toc" "tp" "vr" "vrs")
  "List of known Texinfo exentsions. This list is used by 'texinfo-clean to purge all matching files."
  :safe 'listp)

(defun texinfo-clean ()
  "Remove all Texinfo temporary files. This command should be safe,
but there is no warranty."
  (interactive)
  (let
      ;; Master file.
      ((local-master
        (if (not texinfo-my-masterfile)
            buffer-file-name
          texinfo-my-masterfile)))

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
        texinfo-my-extension-list)))))

(defun texinfo-pdf-view ()
  "Call a PDF viewer for current buffer file. File name should be
properly escaped with double-quotes in case it has spaces."
  (interactive)
  (let
      ;; Master file.
      ((local-master
        (if (not texinfo-my-masterfile)
            buffer-file-name
          texinfo-my-masterfile)))

    (shell-command
     (concat texinfo-my-viewer
             " \""
             (replace-regexp-in-string "\.texi$" "\.pdf" (file-name-nondirectory local-master))
             "\" &" ))
    (delete-windows-on "*Async Shell Command*")))

(defun texinfo-my-menu-update ()
  "Update texinfo node menu automatically."
  (interactive)
  (let
      ;; Master file.
      ((local-master
        (if (not texinfo-my-masterfile)
            buffer-file-name
          texinfo-my-masterfile)))

    (texinfo-multiple-files-update local-master t 8)))

(add-hook
 'texinfo-mode-hook
 (lambda ()
   (setq fill-column 80)
   (setq compilation-scroll-output t)
   (local-set-key (kbd "C-c C-b") 'texinfo-my-menu-update)
   (local-set-key (kbd "C-c C-v") 'texinfo-pdf-view)
   (local-set-key "\C-c\C-t\C-b" 'texinfo-my-compile)))
