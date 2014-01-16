;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun unfill-region ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-region (region-beginning) (region-end) nil)))

(defun sort-lines-unique ()
  "Remove duplicate lines using shell command `sort -u'"
  (interactive)
  (shell-command-on-region (point) (mark) "sort -u" (buffer-name) t))

(defun dtwi () "Delete trailing whitespaces interactively."
  (interactive)
  (query-replace-regexp " +
" "
"))

(defun toggle-window-split ()
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

(define-key my-keys-minor-mode-map [(control c) (|)] 'toggle-window-split)

(defun duplicate (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg
        end
        (origin (point))
        (auto-fill-p (symbol-value 'auto-fill-function)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (auto-fill-mode -1)
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (if auto-fill-p (auto-fill-mode))
      (goto-char (+ origin (* (length region) arg) arg)))))

(define-key my-keys-minor-mode-map (kbd "C-c C-d") 'duplicate)

(defun comment-or-uncomment-current-line-or-region ()
  "Comments or uncomments current current line or whole lines in region."
  (interactive)
  (save-excursion
    (let (min max)
      (if (region-active-p)
          (setq min (region-beginning) max (region-end))
        (setq min (point) max (point)))
      (comment-or-uncomment-region
       (progn (goto-char min) (line-beginning-position))
       (progn (goto-char max) (line-end-position))))))

(define-key my-keys-minor-mode-map "\M-;" 'comment-or-uncomment-current-line-or-region)

(defun xor (b1 b2)
  "Exclusive or of its two arguments."
  (or (and b1 b2)
      (and (not b1) (not b2))))

(defun move-border-left-or-right (arg dir)
  "General function covering move-border-left and
move-border-right. If DIR is t, then move left, otherwise move
right."
  (interactive)
  (if (null arg) (setq arg 5))
  (let ((left-edge (nth 0 (window-edges))))
    (if (xor (= left-edge 0) dir)
        (shrink-window arg t)
      (enlarge-window arg t))))

(defun move-border-left (arg)
  "If this is a window with its right edge being the edge of the
screen, enlarge the window horizontally. If this is a window with
its left edge being the edge of the screen, shrink the window
horizontally. Otherwise, default to enlarging horizontally.

Enlarge/Shrink by ARG columns, or 5 if arg is nil."
  (interactive "P")
  (if (= (count-windows) 2)
      (move-border-left-or-right arg t)))

(defun move-border-right (arg)
  "If this is a window with its right edge being the edge of the
screen, shrink the window horizontally. If this is a window with
its left edge being the edge of the screen, enlarge the window
horizontally. Otherwise, default to shrinking horizontally.

Enlarge/Shrink by ARG columns, or 5 if arg is nil."
  (interactive "P")
  (if (= (count-windows) 2)
      (move-border-left-or-right arg nil)))

(define-key my-keys-minor-mode-map (kbd "M-(") 'move-border-left)
(define-key my-keys-minor-mode-map (kbd "M-)") 'move-border-right)

(defun eval-and-replace ()
  "Replace the last sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (cond ((/= (count-windows) 2)
         (message "You need exactly 2 windows to do this."))
        (t
         (let* ((w1 (first (window-list)))
                (w2 (second (window-list)))
                (b1 (window-buffer w1))
                (b2 (window-buffer w2))
                (s1 (window-start w1))
                (s2 (window-start w2)))
           (set-window-buffer w1 b2)
           (set-window-buffer w2 b1)
           (set-window-start w1 s2)
           (set-window-start w2 s1))))
  (other-window 1))

(define-key my-keys-minor-mode-map (kbd "C-c s") 'swap-windows)

(defun rename-buffer-and-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))

(define-key my-keys-minor-mode-map (kbd "C-x C-w") 'rename-buffer-and-file)

(defun kill-all-buffers ()
  "Kill all buffers, leaving *scratch* only."
  (interactive)
  (mapc
   (lambda (x)
     (kill-buffer x))
   (buffer-list))
  (delete-other-windows))

(defun sanitize ()
  "Untabifies, indents and deletes trailing whitespace from buffer or region."
  (interactive)
  (save-excursion
    (unless (region-active-p)
      (mark-whole-buffer))
    (untabify (region-beginning) (region-end))
    (indent-region (region-beginning) (region-end))
    (save-restriction
      (narrow-to-region (region-beginning) (region-end))
      (delete-trailing-whitespace))))

(defun shell-last-command ()
  "Run last shell command."
  (interactive)
  (let ((last (car shell-command-history)))
    (if last
        (shell-command last)
      (message "Shell command history is empty."))))
(define-key my-keys-minor-mode-map (kbd "C-M-!") 'shell-last-command)

(defun map-on-pair (function sequence)
  "Map a function taking two arguments on a sequence of pairs."
  (mapcar (lambda (p) (funcall function (car p) (cadr p))) sequence ))

(defun mark-word (&optional arg allow-extend)
  "Set mark ARG words away from point.
The place mark goes is the same place \\[forward-word] would move
to with the same argument.  Interactively, if this command is
repeated or (in Transient Mark mode) if the mark is active, it
marks the next ARG words after the ones already marked.

This overloads the vanilla function to mark words from the
beginning."
  (interactive "P\np")
  (cond ((and allow-extend
              (or (and (eq last-command this-command) (mark t))
                  (region-active-p)))
         (setq arg (if arg (prefix-numeric-value arg)
                     (if (< (mark) (point)) -1 1)))
         (set-mark
          (save-excursion
            (goto-char (mark))
            (forward-word arg)
            (point))))
        (t
         ;; The next line makes sure the word at point gets selected if point is
         ;; on the first letter. We need to ignore error if point is at EOF.
         (ignore-errors (forward-char))
         (backward-word)
         (push-mark
          (save-excursion
            (forward-word (prefix-numeric-value arg))
            (point))
          nil t))))

(setq translate-lang-input "")
(setq translate-lang-output "en")
(setq translate-lang-p nil)
(defun translate-set-language ()
  "Set input/output languages for current buffer. Leave input
empty for auto-detect. Empty output defaults to English."
  (interactive)
  (set (make-local-variable 'translate-lang-input)
       (read-from-minibuffer "Input language: "))
  (let ((out (read-from-minibuffer "Output language: ")))
    (set (make-local-variable 'translate-lang-output)
         (if (string-match "^ *$" out) "en" out))))

(defun translate ()
  "Replace current region with its translation."
  (interactive)
  (unless translate-lang-p
    (translate-set-language)
    (set (make-local-variable 'translate-lang-p) t))
  (shell-command-on-region
   (point) (mark)
   (concat "translate "
           (unless (string= translate-lang-input "")
             (concat "-i " translate-lang-input))
           " "  translate-lang-output) nil t))

(defun translate-line-by-line ()
  "Translate lines in region or current line if there is no
region. Output result at the end after an ' = ' separtor."
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
          ;; Shell commands usually outputs an EOL. We should remove it.
          (delete-char -1))
        (setq beg (1+ beg))))))

(defun pos-at-line (arg)
  "Return the position at beginning of line."
  (save-excursion
    (goto-line arg)
    (beginning-of-line)
    (point)))

(defun toggle-trailing-whitespace ()
  (interactive)
  (if show-trailing-whitespace
      (setq show-trailing-whitespace nil)
      (setq show-trailing-whitespace t)))

(defun toggle-indent-tabs ()
  (interactive)
  (if indent-tabs-mode
      (progn
        (message "Indent using spaces")
        (setq indent-tabs-mode nil))
    (message "Indent using tabs")
    (setq indent-tabs-mode t)))

(define-key my-keys-minor-mode-map (kbd "C-c C-i") 'toggle-indent-tabs)

(defun toggle-word-delim ()
  (interactive)
  (if (string= (char-to-string (char-syntax ?_)) "_")
      (progn
        (modify-syntax-entry ?_ "w")
        (message "_ is a not word delimiter"))
    (modify-syntax-entry ?_ "_")
    (message "_ is a word delimiter")))

(defun calc-eval-line ()
  "Compute mathematical expression with calc and append result to
end of line after an ' = ' separtor."
  (interactive)
  (end-of-line)
  (insert " = " (calc-eval (buffer-substring-no-properties
                            (line-beginning-position) (line-end-position)))))

