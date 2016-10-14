;;; PDF utils

(defvar pdf-compressor "pdfcompress" "PDF compressor.")

(defvar pdf-viewer "zathura" "PDF viewer.")


(defvar pdf-viewer-args
  '("--fork"
    "-x" "emacsclient --eval '(progn (switch-to-buffer  (file-name-nondirectory \"%{input}\")) (goto-line %{line}))'")
  "List of arguments passed to `pdf-viewer' when called.
You may want to fork the viewer so that it detects when the same
document is launched twice, and persists when Emacs gets closed.\n
For instance with `zathura':\n
  zathura --fork\n
We can use\n
  emacsclient --eval '(progn (switch-to-buffer  (file-name-nondirectory \"%{input}\")) (goto-line %{line}))'\n
to reverse-search a PDF using SyncTeX. Note this can only work
with emacsclient since we need to communicate the command to an
existing instance.")

;;;###autoload
(defun pdf-compress (&optional file)
  "Call `pdf-compessor' over FILE.
If FILE is not provided, use PDF associated to current buffer
filename. If called with universal argument, prompt for filename.
It FILE is not a PDF, the extension is automatically replaced by
.pdf."
  (interactive
   (list (if (equal current-prefix-arg '(4))
             (expand-file-name (read-file-name "PDF to view: " nil nil t
                                               (concat (file-name-base buffer-file-name) ".pdf"))))))
  (let ((pdf (concat
              (file-name-sans-extension
               (if file file
                 buffer-file-name))
              ".pdf")))
    (when (and (file-exists-p pdf) (file-writable-p pdf)
               (executable-find pdf-compressor))
      (start-process "dummy" nil pdf-compressor "-i" pdf)
      (message "File %s compressed." pdf))))

;;;###autoload
(defun pdf-view (&optional file)
  "Call `pdf-viewer' over FILE.
If FILE is not provided, use PDF associated to current buffer
filename. If called with universal argument, prompt for filename.
It FILE is not a PDF, the extension is automatically replaced by
.pdf."
  (interactive
   (list (if (equal current-prefix-arg '(4))
             (expand-file-name (read-file-name "PDF to view: " nil nil t
                                               (concat (file-name-base buffer-file-name) ".pdf"))))))
  (let ((pdf (concat
              (file-name-sans-extension
               (if file file
                 buffer-file-name))
              ".pdf")))
    (when (and
           (file-exists-p pdf) (file-writable-p pdf)
           (executable-find pdf-viewer))
      (apply 'start-process "dummy" nil pdf-viewer pdf pdf-viewer-args))))

(provide 'tool-pdf)
