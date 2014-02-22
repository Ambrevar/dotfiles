;;; tool-pdf.el --- PDF utils

(defvar pdf-viewer "zathura --fork -s -x \"emacsclient --eval '(progn (switch-to-buffer  (file-name-nondirectory \"'\"'\"%{input}\"'\"'\")) (goto-line %{line}))'\""
  "View PDF associated to current buffer.
You may want to fork the viewer so that it detects when the same
document is launched twice, and persists when Emacs gets closed.\n
Simple command:\n
  zathura --fork\n
We can use\n
  emacsclient --eval '(progn (switch-to-buffer  (file-name-nondirectory \"%{input}\")) (goto-line %{line}))'\n
to reverse-search a PDF using SyncTeX. Note that the quotes and
double-quotes matter and must be escaped appropriately.")

;;;###autoload
(defun pdf-compress (&optional arg)
  "Call external `pdfcompess' inplace.
If no universal argument is provided, use PDF associated to
current buffer filename, ask for filename otherwise."
  (interactive)
  (let ((file (concat
               (file-name-sans-extension
                (if arg arg
                  (if (equal current-prefix-arg '(4))
                      (read-string "File name: " nil nil buffer-file-name)
                    buffer-file-name)))
               ".pdf")))
    (when (and (file-exists-p file) (file-writable-p file))
      ;; TODO: check for errors and print better messages.
      (call-process "pdfcompress" nil nil nil "-i" file))
    ))

;;;###autoload
(defun pdf-view (&optional arg)
  "Call `pdf-viewer' for current buffer file.
If no universal argument is provided, use PDF associated to
current buffer filename, ask for filename otherwise."
  (interactive)
  (let ((file (concat
               (file-name-sans-extension
                (if arg arg
                  (if (equal current-prefix-arg '(4))
                      (read-string "File name: " nil nil buffer-file-name)
                    buffer-file-name)))
               ".pdf")))
    (when (and (file-exists-p file) (file-writable-p file))
      ;; TODO: check for errors and print better messages.
      ;; (call-process "pdfcompress" nil nil nil "-i" file))

      (shell-command
       (concat pdf-viewer
               " \"" file "\" &" ))
      (delete-windows-on "*Async Shell Command*"))))

(provide 'tool-pdf)

;;; tool-pdf.el ends here
