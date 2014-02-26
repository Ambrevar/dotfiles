;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add-hook-and-eval (hook function)
  "Add FUNCTION to HOOK and evaluate it.
This can be useful when called from a hooked function to make
sure it gets executed."
  (add-hook hook function)
  (funcall function))

(defun call-process-to-string (program &rest args)
  "Call PROGRAM with ARGS and return output."
  (with-output-to-string
    (with-current-buffer
        standard-output
      (apply 'call-process program nil t nil args))))

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

(defun calc-eval-line ()
  "Append calc expression to end of line after an ' = ' separtor.
Regular math expression can be computed with calc."
  (interactive)
  (end-of-line)
  (insert " = " (calc-eval (buffer-substring-no-properties
                            (line-beginning-position) (line-end-position)))))

(defun count-occurences (regex string)
  "Return number of times regex occurs in string.
If you want to operate on buffer, use `how-many' instead."
  (let ((start 0) (matches 0))
    (while (string-match regex string start)
      (setq start (match-end 0))
      (setq matches (1+ matches)))
    matches))

(defun count-percents (string)
  "Return count of 'printf' conversion specifications.
Those specifications are introduced by a percent
sign (%). Escaped percent signs (%%) are skipped."
  (let ((start 0) (matches 0))
    (while (string-match "%." string start)
      (unless (string= (match-string 0 string) "%%")
        (setq matches (1+ matches)))
      (setq start (match-end 0)))
    matches))

(defun dtwi ()
  "Delete trailing whitespaces interactively."
  (interactive)
  (query-replace-regexp " +
" "
"))

(defun duplicate (arg)
  "Duplicate the current line or region ARG times.
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
(define-key my-keys-minor-mode-map (kbd "C-x M-d") 'duplicate)

(defun eval-and-replace ()
  "Replace the last sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

;; Fix forward-page. Previously, when the point was at the end of the page,
;; going forward would skip 1 page.  Changed:
;;
;;    (if (bolp) (forward-char 1))
;;
;; to
;;
;;    (if (string= page-delimiter "")
;;
;; I do not know why the (bolp) condition was used since it does not match the
;; above comment. TODO: report this fix!
(defun forward-page (&optional count)
  "Move forward to page boundary.  With arg, repeat, or go back if negative.
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

(defun get-closest-pathname (&optional file)
  "Get pathname of the first instance of FILE towards root.
This may not do the correct thing in presence of links. If it
does not find FILE, then it shall return the name of FILE in the
current directory, suitable for creation"
  (let ((current-dir default-directory) (looping t) (makefile (or file "Makefile")))
    (while (progn
             (if (file-exists-p (expand-file-name makefile current-dir))
                 (setq looping nil)
               (setq current-dir (expand-file-name ".." current-dir)))
             (and looping (not (equal current-dir "/")))))
    (if (equal current-dir "/") nil (expand-file-name makefile current-dir))))

(defun kill-all-buffers ()
  "Kill all buffers, leaving *scratch* only."
  (interactive)
  (mapc
   (lambda (x)
     (kill-buffer x))
   (buffer-list))
  (delete-other-windows))

(defun map-on-pair (function sequence)
  "Map a function taking two arguments on a sequence of pairs."
  (mapcar (lambda (p) (funcall function (car p) (cadr p))) sequence ))

;; TODO: use defadvice instead of duplicate code.
(defun mark-word (&optional arg allow-extend)
  "Set mark ARG words away from point.
The place mark goes is the same place \\[forward-word] would move
to with the same argument.  Interactively, if this command is
repeated or (in Transient Mark mode) if the mark is active, it
marks the next ARG words after the ones already marked.\n
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

(defun move-border-left (arg)
  "Move window border in a natural manner.
If this is a window with its right edge being the edge of the
screen, enlarge the window horizontally. If this is a window with
its left edge being the edge of the screen, shrink the window
horizontally. Otherwise, default to enlarging horizontally.\n
Enlarge/Shrink by ARG columns, or 5 if arg is nil."
  (interactive "P")
  (if (= (count-windows) 2)
      (move-border-left-or-right arg t)))
(define-key my-keys-minor-mode-map (kbd "M-(") 'move-border-left)

(defun move-border-left-or-right (arg dir)
  "Wrapper around move-border-left and move-border-right.
If DIR is t, then move left, otherwise move right."
  (interactive)
  (if (null arg) (setq arg 5))
  (let ((left-edge (nth 0 (window-edges))))
    (if (xor (= left-edge 0) dir)
        (shrink-window arg t)
      (enlarge-window arg t))))

(defun move-border-right (arg)
  "See `move-border-left'."
  (interactive "P")
  (if (= (count-windows) 2)
      (move-border-left-or-right arg nil)))
(define-key my-keys-minor-mode-map (kbd "M-)") 'move-border-right)

(defun page-count ()
  "Return page count.
Requires `count-occurences'."
  (save-restriction
    (widen)
    (save-excursion
      (+ 1 (how-many
            page-delimiter 1 (point-max))))))

(defun page-number ()
  "Reurn page number."
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
        count)
      )))

(defun page-number-mode (activate)
  "Display of page number in mode line.
If ACTIVATE is non-nil, enable, disable otherwise. Interactively,
activate unless called with universal argument.\n\nThis adds
page-number/page-count to mode line. It will only display if
there is more than one page. A page is delimited by
page-delimiter.\n
WARNING: this may slow down editing on big files."
  (interactive (list (not (equal current-prefix-arg '(4)))))
  (setq mode-line-format
 `("%e"
   mode-line-front-space
   mode-line-mule-info
   mode-line-client
   mode-line-modified
   mode-line-remote
   mode-line-frame-identification
   mode-line-buffer-identification
   "   "
   mode-line-position
   ,(when activate '(:eval (when (> (page-count) 1) (format "%d/%d" (page-number) (page-count)))))
   (vc-mode vc-mode)
   "  "
   mode-line-modes
   mode-line-misc-info
   mode-line-end-spaces)))

(defun pos-at-line (arg)
  "Return the position at beginning of line."
  (save-excursion
    (goto-line arg)
    (beginning-of-line)
    (point)))

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
(define-key my-keys-minor-mode-map (kbd "C-x w") 'rename-buffer-and-file)

(defun sanitize ()
  "Untabifies, indents and deletes trailing whitespace.
Works on buffer or region."
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
      (error "Shell command history is empty"))))
(define-key my-keys-minor-mode-map (kbd "C-M-!") 'shell-last-command)

(defun skeleton-make-markers ()
  "Save last skeleton markers in a list.
Hook function for skeletons."
  (while skeleton-markers
    (set-marker (pop skeleton-markers) nil))
  (setq skeleton-markers
        (mapcar 'copy-marker (reverse skeleton-positions))))
;; TODO: skeleton move reverse does not work properly.

(defun skeleton-next-position (&optional reverse)
  "Skeleton movements through placeholders."
  (interactive "P")
  (let ((positions (mapcar 'marker-position skeleton-markers))
        (comp (if reverse '> '<))
        pos)
    (when positions
      (if (catch 'break
            (while (setq pos (pop positions))
              (when (funcall comp (point) pos)
                (throw 'break t))))
          (goto-char pos)
        (goto-char (marker-position
                    (if reverse
                        (car (last skeleton-markers))
                      (car skeleton-markers))))))))

;; Do not expand abbrevs in skeletons. Not sure it is useful.
;; (setq skeleton-further-elements '((abbrev-mode nil)))
(defvar skeleton-markers nil
  "Markers for locations saved in skeleton-positions.")
(add-hook 'skeleton-end-hook 'skeleton-make-markers)
(define-key my-keys-minor-mode-map (kbd "C->") 'skeleton-next-position)
(define-key my-keys-minor-mode-map (kbd "C-<") (lambda () (interactive) (skeleton-next-position t)))

(defun sort-lines-unique ()
  "Remove duplicate lines using shell command `sort -u'."
  (interactive)
  (call-process-region (point) (mark) "sort" t t nil "-u"))

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
(define-key my-keys-minor-mode-map (kbd "C-x M-s") 'swap-windows)

(defun toggle-indent-tabs ()
  "Indent with tabs or spaces."
  (interactive)
  (if indent-tabs-mode
      (progn
        (message "Indent using spaces")
        (setq indent-tabs-mode nil))
    (message "Indent using tabs")
    (setq indent-tabs-mode t)))
(define-key my-keys-minor-mode-map (kbd "C-x M-i") 'toggle-indent-tabs)

(defun toggle-trailing-whitespace ()
  "Show trailing whitespace or not."
  (interactive)
  (if show-trailing-whitespace
      (setq show-trailing-whitespace nil)
    (setq show-trailing-whitespace t)))

(defun toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not.
Run it in each window you want to 'freeze', i.e. prevent Emacs
from acting on it."
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window
                                 (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))
(define-key my-keys-minor-mode-map [pause] 'toggle-window-dedicated)

(defun toggle-window-split ()
  "Switch between vertical and horizontal split.
It only works for frames with exactly two windows."
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
(define-key my-keys-minor-mode-map [(control x) (|)] 'toggle-window-split)

(defun toggle-word-delim ()
  "Make underscore part of the word syntax or not.
This does not interfere with `subword-mode'."
  (interactive)
  (if (string= (char-to-string (char-syntax ?_)) "_")
      (progn
        (modify-syntax-entry ?_ "w")
        (message "_ is a not word delimiter"))
    (modify-syntax-entry ?_ "_")
    (message "_ is a word delimiter")))

(defun unfill-paragraph ()
  "Paragraph at point is unwrapped on one single line."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun unfill-region ()
  "Unfill all paragraphs found in current region.
Each paragraph stand on its line."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-region (region-beginning) (region-end) nil)))

(defun xor (b1 b2)
  "Exclusive or of its two arguments."
  (or (and b1 b2)
      (and (not b1) (not b2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'functions)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
