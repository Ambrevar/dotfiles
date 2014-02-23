;;; tool-translate.el --- Translation framework

(defvar translate-lang-input ""
  "Current input language for the `translate' function.
Change it with `translate-set-language'.")

(defvar translate-lang-output "en"
  "Current output language for the `translate' function.
Change it with `translate-set-language'.")

(defvar translate-lang-p nil
  "If language has been set for translation.
The original value is nil. When `translate' is called for the
first time, input and output languages are set and this variable
is set to true.")

;; TODO: use call-process
;; TODO: check if region, if not do sth else
;;;###autoload
(defun translate ()
  "Replace current region with its translation."
  (interactive)
  (cond ((not (mark)) (error "Mark not set"))
        ((not (executable-find "translate")) (error "Program `translate' not found in path"))
        (t
         (unless translate-lang-p
           (translate-set-language)
           (set (make-local-variable 'translate-lang-p) t))
         (shell-command-on-region
          (point) (mark)
          (concat "translate "
                  (unless (string= translate-lang-input "")
                    (concat "-i " translate-lang-input))
                  " "  translate-lang-output) nil t))))



;;;###autoload
(defun translate-line-by-line ()
  "Translate current line or lines in region.
This calls the `translate' function.  Output result at the end
after an ' = ' separtor."
  (interactive)
  (unless translate-lang-p
    (translate-set-language)
    (set (make-local-variable 'translate-lang-p) t))

  (let ((line)
        (cmd (concat "translate "
                     (unless (string= translate-lang-input "")
                       (concat "-i " translate-lang-input))
                     " "  translate-lang-output))
        (beg (line-number-at-pos (point)))
        (end (line-number-at-pos (if mark-active (mark) (point)))))

    ;; Mark is assumed to specify the end. If it not not the case, we switch
    ;; the values.
    (when (> beg end)
      (setq beg (line-number-at-pos (mark)))
      (setq end (line-number-at-pos (point))))

    (save-excursion
      ;; forward-line will remain on the same line if EOF has been reached. Need
      ;; to check for it.
      (while (<= beg end)
        (goto-line beg)
        (setq line (buffer-substring-no-properties
                    (line-beginning-position) (line-end-position)))
        (unless (string-match "^ *$" line)
          (end-of-line)
          (insert " = " (shell-command-to-string
                         (concat cmd " '" line "'")))
          ;; Shell commands usually output an EOL. We should remove it.
          (delete-char -1))
        (setq beg (1+ beg))))))

;;;###autoload
(defun translate-set-language ()
  "Set input/output translation languages for current buffer.
These languages are used by `translate'. Leave input empty for
auto-detect. Empty output defaults to English."
  (interactive)
  (set (make-local-variable 'translate-lang-input)
       (read-from-minibuffer "Input language: "))
  (let ((out (read-from-minibuffer "Output language: ")))
    (set (make-local-variable 'translate-lang-output)
         (if (string-match "^ *$" out) "en" out))))

(provide 'tool-translate)

;;; tool-translate.el ends here
