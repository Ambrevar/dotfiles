;;; Itranslate
;; Translate text from Internet services

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
     " *\n$" ""
     (call-process-to-string
      itranslate-program
      in out str))))

;;;###autoload
(defun itranslate (str &optional insert in out)
  "Output translated STR to minibuffer.
If INSERT is non-nil or if called with universal argument, insert
result at point. If IN or OUT are nil, use
`itranslate-lang-input' and `itranslate-lang-output'
respectively."
  (interactive
   (list (read-string "Text to translate: ")
         (equal current-prefix-arg '(4))))
  (when (called-interactively-p 'any) (itranslate-init))
  (let ((lang-in (or in itranslate-lang-input))
        (lang-out (or out itranslate-lang-output)))
    (if insert
        (insert (itranslate-string str lang-in lang-out))
      (message "%s" (itranslate-string str lang-in lang-out)))))

;;;###autoload
(defun itranslate-region (beg end &optional replace in out)
  "Return translated region to minibuffer.
If no region is set, return an error. If REPLACE is non-nil or if
called with universal argument, insert result at point. If IN or
OUT are nil, use `itranslate-lang-input' and
`itranslate-lang-output' respectively."
  (interactive
   (list (if mark-active (mark) (error "Mark not set"))
         (point)
         (equal current-prefix-arg '(4))))
  (when (called-interactively-p 'any) (itranslate-init))
  (when (> beg end)
    (setq beg (point))
    (setq end (mark)))
  (let ((lang-in (or in itranslate-lang-input))
        (lang-out (or out itranslate-lang-output))
        (text (buffer-substring-no-properties beg end)))
    (if (not replace)
        (message (itranslate-string text lang-in lang-out))
      (delete-region beg end)
      (insert (itranslate-string text lang-in lang-out)))))

;;;###autoload
(defun itranslate-lines (beg end &optional in out)
  "Translate lines between BEG and END.
Interactively, lines in region are used, or current line if no
region. This calls the `itranslate' function. Output result at
the end after an ' = ' separtor."
  (interactive
   (list (line-number-at-pos (point))
         (line-number-at-pos (if mark-active (mark) (point))) nil nil))
  (when (called-interactively-p 'any) (itranslate-init))
  (when (> beg end)
    (setq beg end)
    (setq end (line-number-at-pos (point))))
  (save-excursion
    ;; forward-line will remain on the same line if EOF has been reached. Need
    ;; to check for it.
    (let (line
          (lang-in (or in itranslate-lang-input))
          (lang-out (or out itranslate-lang-output)))
      (while (<= beg end)
        (forward-line beg)
        (setq line (buffer-substring-no-properties
                    (line-beginning-position) (line-end-position)))
        (unless (string-match "^ *$" line)
          (end-of-line)
          (insert " = " (itranslate line in out)))
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
       (read-from-minibuffer
        "Input language (leave empty for auto-detect): " itranslate-lang-input))
  (set (make-local-variable 'itranslate-lang-output)
       (read-string "Output language: " itranslate-lang-output)))

(provide 'tool-itranslate)
