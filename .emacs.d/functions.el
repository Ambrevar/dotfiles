;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;==============================================================================
;; Unfill paragraph
;;==============================================================================

(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun unfill-region ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-region (region-beginning) (region-end) nil)))


;; Remove duplicate lines. (sort -u)
(defun remove-dup ()
  (interactive)
  (shell-command-on-region (point) (mark) "sort -u" (buffer-name) t)
)

(defun dtwi () "Delete trailing whitespaces interactively."
  (interactive)
  (query-replace-regexp " +
" "
")
)

(defun list-buffers-switch () "Same as list buffers but switch to it afterward."
  (interactive)
  (list-buffers)
  (switch-to-buffer-other-window "*Buffer List*")
)
(define-key my-keys-minor-mode-map (kbd "C-x C-b") 'list-buffers-switch)

;;==============================================================================
;; Toggle window split
;;==============================================================================
(defun my-toggle-window-split ()
  "Vertical split shows more of each line, horizontal split shows
more lines. This code toggles between them. It only works for
frames with exactly two windows."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(define-key my-keys-minor-mode-map [(control c) (|)] 'my-toggle-window-split)

;;==============================================================================
;; Duplicate line
;;==============================================================================
(defun duplicate-line (arg)
  "Duplicate current line, leaving point in lower line."
  (interactive "*p")

  ;; save the point for undo
  (setq buffer-undo-list (cons (point) buffer-undo-list))

  ;; local variables for start and end of line
  (let ((bol (save-excursion (beginning-of-line) (point)))
        eol)
    (save-excursion

      ;; don't use forward-line for this, because you would have
      ;; to check whether you are at the end of the buffer
      (end-of-line)
      (setq eol (point))

      ;; store the line and disable the recording of undo information
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t)
            (count arg))
        ;; insert the line arg times
        (while (> count 0)
          (newline)         ;; because there is no newline in 'line'
          (insert line)
          (setq count (1- count)))
        )

      ;; create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list)))
    ) ; end-of-let

  ;; put the point in the lowest line and return
  (next-line arg))

;; Binding.
(define-key my-keys-minor-mode-map (kbd "C-c C-d") 'duplicate-line)

;;==============================================================================
;; Comment DWIM -- toggle comment line
;;==============================================================================

;; Original idea from
;; http://www.opensubscriber.com/message/emacs-devel@gnu.org/10971693.html
(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command. If no region is
selected and current line is not blank and we are not at the end
of the line, then comment current line.  Replaces default
behaviour of comment-dwim, when it inserts comment at the end of
the line."
  (interactive "*P")
  (comment-normalize-vars)
  ;; (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
  (if (and (not (region-active-p)) )
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

;; Binding.
(define-key my-keys-minor-mode-map "\M-;" 'comment-dwim-line)
