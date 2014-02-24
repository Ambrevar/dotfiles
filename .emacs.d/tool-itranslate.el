;;; tool-itranslate.el --- Internet Translation

(defvar itranslate-program "translate"
  "The translation program installed on the system.")

(defvar itranslate-lang-input ""
  "Current input language for the `itranslate' function.
Change it with `itranslate-set-language'.")

(defvar itranslate-lang-output "en"
  "Current output language for the `itranslate' function.
Change it with `itranslate-set-language'.")

(defvar itranslate-lang-p nil
  "If language has been set for current buffer.
The original value is nil. When any itranslate function is called
for the first time, input and output languages are set and this
variable is set to true.")

(defun itranslate-string (str in out)
  "Return STR translated from IN language to OUT language.
Requires `call-process-to-string' from `functions'."
  (require 'functions)
  (if (not (executable-find itranslate-program))
      (error "Program `%s' not found in path" itranslate-program)
    (replace-regexp-in-string
     "\n$" ""
     (call-process-to-string
      itranslate-program
      "-i"
      in out str))))

;;;###autoload
(defun itranslate (str &optional insert in out)
  "Output translate STR to minibuffer.
If INSERT is non-nil or called with universal argument, insert
result at point. If IN or OUT are nil, use
`itranslate-lang-input' and `itranslate-lang-output'
respectively."
  (interactive
   (list (read-string "Text to translate: ")
         (equal current-prefix-arg '(4))))
  (when (called-interactively-p 'any) (itranslate-init))
  (let ((lang-in (or in itranslate-lang-input))
        (lang-out (or out itranslate-lang-input)))
    (if insert
        (insert (itranslate-string str lang-in lang-out))
      (message (itranslate-string str lang-in lang-out)))))

;; TODO: rewrite this one.
;;;###autoload
(defun itranslate-lines (&option in out)
  "Translate current line or lines in region.
This calls the `itranslate' function.  Output result at the end
after an ' = ' separtor."
  (interactive)
  (unless itranslate-lang-p
    (itranslate-set-language)
    (set (make-local-variable 'itranslate-lang-p) t))

  (let ((line)
        (cmd (concat itranslate-program " "
                     (unless (string= itranslate-lang-input "")
                       (concat "-i " itranslate-lang-input))
                     " "  itranslate-lang-output))
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

(defun itranslate-init ()
  "Call `itranslate-set-language' if not yet called for current buffer."
  (unless itranslate-lang-p
    (itranslate-set-language)
    (set (make-local-variable 'itranslate-lang-p) t)))

;;;###autoload
(defun itranslate-set-language ()
  "Set input/output translation languages for current buffer.
These languages are used by `itranslate'. Leave input empty for
auto-detect. Empty output defaults to English."
  (interactive)
  (set (make-local-variable 'itranslate-lang-input)
       (read-from-minibuffer "Input language: "))
  (set (make-local-variable 'itranslate-lang-output)
       (read-string "Output language: " nil nil "en")))

(provide 'tool-itranslate)

;;; tool-itranslate.el ends here
