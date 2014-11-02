;;==============================================================================
;; Lua
;;==============================================================================

(defvaralias 'lua-indent-level 'tab-width)

(add-hook-and-eval
 'lua-mode-hook
 (lambda ()
   (set (make-local-variable 'compile-command) (concat "lua " buffer-file-name))))


(defun lua-make-indentation-info-pair (found-token found-pos)
  "This is a helper function to lua-calculate-indentation-info. Don't
use standalone.

This is function has been overloaded from `lua-mode'."
  (cond
   ;; function is a bit tricky to indent right. They can appear in a lot ot
   ;; different contexts. Until I find a shortcut, I'll leave it with a simple
   ;; relative indentation.
   ;; The special cases are for indenting according to the location of the
   ;; function. i.e.:
   ;;       (cons 'absolute (+ (current-column) lua-indent-level))
   ;; TODO: Fix this. It causes really ugly indentations for in-line functions.
   ((string-equal found-token "function")
    (cons 'relative lua-indent-level))

   ;; block openers
   ((member found-token (list "{" "(" "["))
    ;; WARNING: Changed here: remove alignment.
    (cons 'relative lua-indent-level))

   ;; These are not really block starters. They should not add to indentation.
   ;; The corresponding "then" and "do" handle the indentation.
   ((member found-token (list "if" "for" "while"))
    (cons 'relative 0))
   ;; closing tokens follow: These are usually taken care of by
   ;; lua-calculate-indentation-override.
   ;; elseif is a bit of a hack. It is not handled separately, but it needs to
   ;; nullify a previous then if on the same line.
   ((member found-token (list "until" "elseif"))
    (save-excursion
      (let ((line (line-number-at-pos)))
        (if (and (lua-goto-matching-block-token nil found-pos 'backward)
                 (= line (line-number-at-pos)))
            (cons 'remove-matching 0)
          (cons 'relative 0)))))

   ;; else is a special case; if its matching block token is on the same line,
   ;; instead of removing the matching token, it has to replace it, so that
   ;; either the next line will be indented correctly, or the end on the same
   ;; line will remove the effect of the else.
   ((string-equal found-token "else")
    (save-excursion
      (let ((line (line-number-at-pos)))
        (if (and (lua-goto-matching-block-token nil found-pos 'backward)
                 (= line (line-number-at-pos)))
            (cons 'replace-matching (cons 'relative lua-indent-level))
          (cons 'relative lua-indent-level)))))

   ;; Block closers. If they are on the same line as their openers, they simply
   ;; eat up the matching indentation modifier. Otherwise, they pull
   ;; indentation back to the matching block opener.
   ((member found-token (list ")" "}" "]" "end"))
    (save-excursion
      (let ((line (line-number-at-pos))
            (cur-point (point)))
        (back-to-indentation)
        (lua-find-regexp 'forward lua-indentation-modifier-regexp (line-end-position))
        (back-to-indentation)
        (let ((first-token-p (and (= (match-beginning 0) (point))
                                  (= (lua-find-regexp 'forward lua-indentation-modifier-regexp (line-end-position)) cur-point))))
          (lua-goto-matching-block-token nil found-pos 'backward)
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

(defun lua-calculate-indentation-override (&optional parse-start)
  "Return overriding indentation amount for special cases.
Look for an uninterrupted sequence of block-closing tokens that starts
at the beginning of the line. For each of these tokens, shift indentation
to the left by the amount specified in lua-indent-level.

This is function has been overloaded from `lua-mode'."
  (let ((indentation-modifier 0)
        (case-fold-search nil)
        (block-token nil))
    (save-excursion
      (if parse-start (goto-char parse-start))
      ;; Look for the last block closing token
      (back-to-indentation)
      (when (and (not (lua-comment-or-string-p))
                 (looking-at lua-indentation-modifier-regexp)
                 (let ((token-info (lua-get-block-token-info (match-string 0))))
                   (and token-info
                        (not (eq 'open (lua-get-token-type token-info))))))
        (lua-forward-line-skip-blanks 'back)
        (- (+ (cdr (lua-accumulate-indentation-info (lua-calculate-indentation-info-1 indentation-info (line-end-position)))) (current-indentation)) lua-indent-level)))))


(provide 'mode-lua)
