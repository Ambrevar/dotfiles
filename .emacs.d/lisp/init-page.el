;;; page-number-mode

(defvar page-number--mode-line-position-without-page nil)

(defvar page-number--position
  '(
    ((column-number-mode
      (14 (:eval (propertize
                  (format " (%%l,%%c,%d/%d)" (page-number) (page-count))
                  'local-map mode-line-column-line-number-mode-map
                  'mouse-face 'mode-line-highlight
                  'help-echo "Line number and Column number\n\
mouse-1: Display Line and Column Mode Menu")))
      (10 (:eval (propertize
                  (format " L%%l,%d/%d" (page-number) (page-count))
                  'local-map mode-line-column-line-number-mode-map
                  'mouse-face 'mode-line-highlight
                  'help-echo "Line Number\n\
mouse-1: Display Line and Column Mode Menu")))))
    ((column-number-mode
      (9 (:eval (propertize
                 (format " C%%c,%d/%d" (page-number) (page-count))
                 'local-map mode-line-column-line-number-mode-map
                 'mouse-face 'mode-line-highlight
                 'help-echo "Column number\n\
mouse-1: Display Line and Column Mode Menu")))))))

;;; This is different from `what-page' that returns a descriptive string.
(defun page-count ()
  "Return page count."
  (save-restriction
    (widen)
    (save-excursion
      (1+ (how-many
           page-delimiter 1 (point-max))))))

(defun page-number ()
  "Return page number."
  (save-restriction
    (widen)
    (save-excursion
      (let ((count 1)
            (opoint (point)))
        (goto-char (point-min))
        (while (re-search-forward page-delimiter opoint t)
          (if (= (match-beginning 0) (match-end 0))
              (forward-char 1))
          (setq count (1+ count)))
        count))))

(define-minor-mode page-number-mode
  "Toggle page number display in the mode line (Page Number mode).
With a prefix argument ARG, enable Page Number mode if ARG is
positive, and disable it otherwise.

If called from Lisp, enable the mode if ARG is omitted or nil.

It will only display if there is more than one page. A page is
delimited by ‘page-delimiter’.

WARNING: this may slow down editing on big files."
  :global t :group 'mode-line
  (let ((l mode-line-position))
    (while (not (eq (caar l) 'line-number-mode))
      (setq l (cdr l)))
    (if (not page-number-mode)
        ;; Restore.
        (setcdr (car l) page-number--mode-line-position-without-page)
      ;; Set.
      (unless page-number--mode-line-position-without-page
        (setq page-number--mode-line-position-without-page (cdar l)))
      (setcdr (car l) page-number--position))))



;;; REVIEW: Fix forward-page. Previously, when the point was at the end of the
;;; page, going forward would skip 1 page.  Changed:
;;
;;    (if (bolp) (forward-char 1))
;;
;;; to
;;
;;    (if (string= page-delimiter "")
;;
;;; I do not know why the (bolp) condition was used since it does not match the
;;; above comment.
;;; Reported at http://debbugs.gnu.org/cgi/bugreport.cgi?bug=20663.
(defun forward-page (&optional count)
  "Move forward to page boundary.
With prefix or COUNT, repeat, or go back if negative.
A page boundary is any line whose beginning matches the regexp
`page-delimiter'."
  (interactive "p")
  (or count (setq count 1))
  (while (and (> count 0) (not (eobp)))
    ;; In case the page-delimiter matches the null string,
    ;; don't find a match without moving.
    (if (string= page-delimiter "") (forward-char 1))
    (if (re-search-forward page-delimiter nil t)
        nil
      (goto-char (point-max)))
    (setq count (1- count)))
  (while (and (< count 0) (not (bobp)))
    ;; In case the page-delimiter matches the null string,
    ;; don't find a match without moving.
    (and (save-excursion (re-search-backward page-delimiter nil t))
         (= (match-end 0) (point))
         (goto-char (match-beginning 0)))
    (forward-char -1)
    (if (re-search-backward page-delimiter nil t)
        ;; We found one--move to the end of it.
        (goto-char (match-end 0))
      ;; We found nothing--go to beg of buffer.
      (goto-char (point-min)))
    (setq count (1+ count))))

(provide 'init-page)
