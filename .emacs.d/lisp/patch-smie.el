;;; Hack around SMIE
;;; This was intended to fix some undesirable behaviour related to forced alignment in indentation with Lua.
;;; See https://github.com/immerrr/lua-mode/issues/31.
;;; 2017-10-27: Not sure this is still valid.
(require 'smie)

(defcustom smie-align nil "Whether to align function arguments.")

(defun smie-indent-exps ()
  "Indentation of sequences of simple expressions without intervening keywords or operators.
E.g. 'a b c' or `g (balbla) f'. Can be a list of expressions or a
function call. If it's a function call, the first element is
special (it's the function). We distinguish function calls from
mere lists of expressions based on whether the preceding token is
listed in the `list-intro' entry of smie-indent-rules.

Overload original `smie-indent-exps': make alignment customizable
in `smie-align'."

  ;; TODO: to indent Lisp code, we should add a way to specify
  ;; particular indentation for particular args depending on the
  ;; function (which would require always skipping back until the
  ;; function).
  ;; TODO: to indent C code, such as "if (...) {...}" we might need
  ;; to add similar indentation hooks for particular positions, but
  ;; based on the preceding token rather than based on the first exp.
  (save-excursion
    (let ((positions nil)
          arg)
      (while (and (null (car (smie-backward-sexp)))
                  (push (point) positions)
                  (not (smie-indent--bolp))))
      (save-excursion
        ;; Figure out if the atom we just skipped is an argument rather
        ;; than a function.
        (setq arg
              (or (null (car (smie-backward-sexp)))
                  (funcall smie-rules-function :list-intro
                           (funcall smie-backward-token-function)))))
      (cond
       ((null positions)
        ;; We're the first expression of the list.  In that case, the
        ;; indentation should be (have been) determined by its context.
        nil)
       (arg
        ;; There's a previous element, and it's not special (it's not
        ;; the function), so let's just align with that one.
        (goto-char (car positions))
        (current-column))
       ((cdr positions)
        ;; We skipped some args plus the function and bumped into something.
        (if (not smie-align)
            ;; Indent once.
            (+ (smie-indent--offset 'args) (current-column))
          ;; Align with the first arg.
          (goto-char (cadr positions))
          (current-column)))
       (positions
        ;; We're the first arg.
        (goto-char (car positions))
        (+ (smie-indent--offset 'args)
           ;; We used to use (smie-indent-virtual), but that
           ;; doesn't seem right since it might then indent args less than
           ;; the function itself.
           (current-column)))))))

(provide 'patch-smie)
