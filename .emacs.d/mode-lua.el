;;==============================================================================
;; Lua
;;==============================================================================

(defvaralias 'lua-indent-level 'tab-width)

(add-hook-and-eval
 'lua-mode-hook
 (lambda ()
   (set (make-local-variable 'compile-command) (concat "lua " buffer-file-name))))


;; The following hacks up lua-mode indentation.

(defun lua-make-indentation-info-pair (found-token found-pos)
  "This is a helper function to lua-calculate-indentation-info. Don't
use standalone."
  (cond
   ;; Block openers.
   ((member found-token (list "{" "(" "[" "function"))
    (cons 'relative lua-indent-level))

   ;; These are not really block starters. They should not add to indentation.
   ;; The corresponding "then" and "do" handle the indentation.
   ((member found-token (list "if" "for" "while"))
    (cons 'relative 0))

   ;; Closing tokens follow: These are usually taken care of by
   ;; lua-calculate-indentation-override.
   ;; elseif is a bit of a hack. It is not handled separately, but it needs to
   ;; nullify a previous then if on the same line.
   ((member found-token (list "until" "elseif"))
    (save-excursion
      (let ((line (line-number-at-pos)))
        (if (and (lua-goto-matching-block-token found-pos 'backward)
                 (= line (line-number-at-pos)))
            (cons 'remove-matching 0)
          (cons 'relative 0)))))

   ;; 'else' is a special case; if its matching block token is on the same line,
   ;; instead of removing the matching token, it has to replace it, so that
   ;; either the next line will be indented correctly, or the end on the same
   ;; line will remove the effect of the else.
   ((string-equal found-token "else")
    (save-excursion
      (let ((line (line-number-at-pos)))
        (if (and (lua-goto-matching-block-token found-pos 'backward)
                 (= line (line-number-at-pos)))
            (cons 'replace-matching (cons 'relative lua-indent-level))
          (cons 'relative lua-indent-level)))))

   ;; Block closers. If they are on the same line as their openers, they simply
   ;; eat up the matching indentation modifier. Otherwise, they pull indentation
   ;; back to the matching block opener. Special case if the token is at
   ;; beginning of line: since `lua-calculate-indentation-override' unindented
   ;; the line by one level already, then it only eats up the matching
   ;; indentation midifier.
   ((member found-token (list ")" "}" "]" "end"))
    (save-excursion
      (let ((line (line-number-at-pos))
            (cur-point (point)))
        (back-to-indentation)
        (lua-find-regexp 'forward lua-indentation-modifier-regexp (line-end-position))
        (back-to-indentation)
        (let ((first-token-p (and (= (match-beginning 0) (point))
                                  (= (lua-find-regexp 'forward lua-indentation-modifier-regexp (line-end-position)) cur-point))))
          (lua-goto-matching-block-token found-pos 'backward)
          (if (and (/= line (line-number-at-pos)) (not first-token-p))
              (cons 'relative (- lua-indent-level))
            (cons 'remove-matching 0))))))

   ;; Everything else. This is from the original code: If opening a block
   ;; (match-data 1 exists), then push indentation one level up, if it is
   ;; closing a block, pull it one level down.
   ('other-indentation-modifier
    (cons 'relative (if (nth 2 (match-data))
                        ;; beginning of a block matched
                        lua-indent-level
                      ;; end of a block matched
                      (- lua-indent-level))))))

(defun lua-calculate-indentation-info (&optional parse-end)
  "For each block token on the line, computes how it affects the indentation.
The effect of each token can be either a shift relative to the current
indentation level, or indentation to some absolute column. This information
is collected in a list of indentation info pairs, which denote absolute
and relative each, and the shift/column to indent to."
  (lua-calculate-indentation-info-1
   (list (cons 'absolute (current-indentation)))
   (min parse-end (line-end-position))))

(defun lua-calculate-indentation-override (&optional parse-start)
  "Return overriding indentation amount for special cases.
Look for an uninterrupted sequence of block-closing tokens that starts
at the beginning of the line. For each of these tokens, shift indentation
to the left by the amount specified in lua-indent-level."
  (let ((indentation-modifier 0)
        (indentation-info (list (cons 'absolute 0)))
        (case-fold-search nil))
    (save-excursion
      (if parse-start (goto-char parse-start))
      ;; Look for the last block closing token
      (back-to-indentation)
      (when (and (not (lua-comment-or-string-p))
                 (looking-at lua-indentation-modifier-regexp)
                 (let ((token-info (lua-get-block-token-info (match-string 0))))
                   (and token-info
                        (not (eq 'open (lua-get-token-type token-info))))))
        ;; Compute indentation changes induced by unmatched closers on current line.
        (while (lua-find-regexp 'forward lua-indentation-modifier-regexp (line-end-position))
          (when (and (not (lua-comment-or-string-p))
                     (looking-at lua-indentation-modifier-regexp)
                     (let ((token-info (lua-get-block-token-info (match-string 0))))
                       (and token-info
                            (not (eq 'open (lua-get-token-type token-info))))))
            (setq indentation-modifier (- indentation-modifier lua-indent-level))))
        ;; Current line indentation is:
        ;; + indentation changes induced by unmatched closers on current line
        ;; + indentation changes induced by previous line
        ;; + previous line indentation
        ;; - indent-level if previous line was a continuation.
        ;; - indent-level
        (lua-forward-line-skip-blanks 'back)
        (- (+ indentation-modifier
              (cdr (lua-accumulate-indentation-info (lua-calculate-indentation-info-1 indentation-info (line-end-position))))
              (current-indentation))
           ;; Previous line is a continuing statement, but not current.
           (if (lua-is-continuing-statement-p)
               lua-indent-level
             0)
           lua-indent-level)))))

(defun lua-calculate-indentation (&optional parse-start)
  "Return appropriate indentation for current line as Lua code."
  (save-excursion
    (let ((continuing-p (lua-is-continuing-statement-p))
          (cur-line-begin-pos (line-beginning-position)))
      (or
       ;; First, check if the line starts with a closer. If so, it should be
       ;; indented/unindented in special way
       (lua-calculate-indentation-override)

       (when (lua-forward-line-skip-blanks 'back)
         ;; The order of function calls here is important. Block modifier
         ;; call may change the point to another line.
         (let ((modifier
                 (lua-calculate-indentation-block-modifier cur-line-begin-pos)))
           (+ (current-indentation)
              modifier

              ;; Previous line is a continuing statement, but not current.
              (if (and (lua-is-continuing-statement-p) (not continuing-p))
                  (- lua-indent-level)
                0)
              
              ;; Current line is a continuing statement, but not previous.
              (if (and (not (lua-is-continuing-statement-p)) continuing-p)
                  lua-indent-level
                0))))

       ;; If there's no previous line, indentation is 0.
       0))))

(provide 'mode-lua)
