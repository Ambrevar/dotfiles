;;==============================================================================
;; SGML
;;==============================================================================

;; Just because XML is ugly.
(add-hook-and-eval
 'sgml-mode-hook
 (lambda ()
   (setq sgml-xml-mode t)
   (turn-off-auto-fill)
   (toggle-truncate-lines)))

;; We redefine sgml-calculate-indent to remove alignment.
(defun sgml-calculate-indent (&optional lcon)
  "Calculate the column to which this line should be indented.
LCON is the lexical context, if any."
  (unless lcon (setq lcon (sgml-lexical-context)))

  ;; Indent comment-start markers inside <!-- just like comment-end markers.
  (if (and (eq (car lcon) 'tag)
           (looking-at "--")
           (save-excursion (goto-char (cdr lcon)) (looking-at "<!--")))
      (setq lcon (cons 'comment (+ (cdr lcon) 2))))

  (pcase (car lcon)

    (`string
     ;; Go back to previous non-empty line.
     (while (and (> (point) (cdr lcon))
                 (zerop (forward-line -1))
                 (looking-at "[ \t]*$")))
     (if (> (point) (cdr lcon))
         ;; Previous line is inside the string.
         (current-indentation)
       (goto-char (cdr lcon))
       (1+ (current-column))))

    (`comment
     (let ((mark (looking-at "--")))
       ;; Go back to previous non-empty line.
       (while (and (> (point) (cdr lcon))
                   (zerop (forward-line -1))
                   (or (looking-at "[ \t]*$")
                       (if mark (not (looking-at "[ \t]*--"))))))
       (if (> (point) (cdr lcon))
           ;; Previous line is inside the comment.
           (skip-chars-forward " \t")
         (goto-char (cdr lcon))
         ;; Skip `<!' to get to the `--' with which we want to align.
         (search-forward "--")
         (goto-char (match-beginning 0)))
       (when (and (not mark) (looking-at "--"))
         (forward-char 2) (skip-chars-forward " \t"))
       (current-column)))

    ;; We don't know how to indent it.  Let's be honest about it.
    (`cdata nil)
    ;; We don't know how to indent it.  Let's be honest about it.
    (`pi nil)

    (`tag
     ;; WARNING: This has been modified.
     (goto-char (cdr lcon))
     (+ (current-column) sgml-basic-offset))

    (`text
     (while (looking-at "</")
       (forward-sexp 1)
       (skip-chars-forward " \t"))
     (let* ((here (point))
            (unclosed (and ;; (not sgml-xml-mode)
                       (looking-at sgml-tag-name-re)
                       (assoc-string (match-string 1)
                                     sgml-unclosed-tags 'ignore-case)
                       (match-string 1)))
            (context
             ;; If possible, align on the previous non-empty text line.
             ;; Otherwise, do a more serious parsing to find the
             ;; tag(s) relative to which we should be indenting.
             (if (and (not unclosed) (skip-chars-backward " \t")
                      (< (skip-chars-backward " \t\n") 0)
                      (back-to-indentation)
                      (> (point) (cdr lcon)))
                 nil
               (goto-char here)
               (nreverse (sgml-get-context (if unclosed nil 'empty)))))
            (there (point)))
       ;; Ignore previous unclosed start-tag in context.
       (while (and context unclosed
                   (eq t (compare-strings
                          (sgml-tag-name (car context)) nil nil
                          unclosed nil nil t)))
         (setq context (cdr context)))
       ;; Indent to reflect nesting.
       (cond
        ;; If we were not in a text context after all, let's try again.
        ((and context (> (sgml-tag-end (car context)) here))
         (goto-char here)
         (sgml-calculate-indent
          (cons (if (memq (sgml-tag-type (car context)) '(comment cdata))
                    (sgml-tag-type (car context)) 'tag)
                (sgml-tag-start (car context)))))
        ;; Align on the first element after the nearest open-tag, if any.
        ((and context
              (goto-char (sgml-tag-end (car context)))
              (skip-chars-forward " \t\n")
              (< (point) here) (sgml-at-indentation-p))
         (current-column))
        (t
         (goto-char there)
         (+ (current-column)
            (* sgml-basic-offset (length context)))))))

    (_
     (error "Unrecognized context %s" (car lcon)))))

(provide 'mode-sgml)
