;; Functions

;; Note on mark and region: to get a consistent behaviour whether transient mode
;; is on or off, check `mark-active' to know if the region is active. It will
;; work as expected if transient. If not, it will always be true as soon as the
;; mark has been set once; so you need to make sure the mark is set as you want
;; beforehand (e.g. whole buffer, single line...). This is the behaviour of
;; `sort-lines'.

(defun add-hook-and-eval (hook function)
  "Add FUNCTION to HOOK and evaluate it.
This can be useful when called from a hooked function to make
sure it gets executed."
  (add-hook hook function)
  (funcall function))

(defun calc-eval-line ()
  "Append calc expression to end of line after an ' = ' separtor.
Regular math expression can be computed with calc."
  (interactive)
  (end-of-line)
  (insert " = " (calc-eval (buffer-substring-no-properties
                            (line-beginning-position) (line-end-position)))))

(defun call-process-to-string (program &rest args)
  "Call PROGRAM with ARGS and return output."
  (with-output-to-string
    (with-current-buffer
        standard-output
      (apply 'call-process program nil t nil args))))

;; (defcustom compilation-after-hook nil
;;   "List of hook functions run by `compile-custom'."
;;   :type 'hook
;;   :group 'compilation)

(defcustom compilation-before-hook nil
  "List of hook functions run by `compile-custom'.
You may want to set the `compile-command' with this hook. If you
do so, do not forget to set the LOCAL flag to t."
  :type 'hook
  :group 'compilation)

(defvar compilation-time-before-hide-window nil
  "Hide compilation window after the specified seconds.
If nil, do not hide.")

(defun compile-here (&optional runhooks)
  "Call `recompile' in the default directory.
If RUNHOOKS is non-nil (or with \\[universal-argument]), run hooks in
`compilation-before-hook', then `recompile'."
  (interactive "P")
  (when (or runhooks (string= compile-command "make -k ")) (run-hooks 'compilation-before-hook))
  (setq-default compilation-directory default-directory)
  (recompile)
  (when compilation-time-before-hide-window
    (sit-for compilation-time-before-hide-window)
    (delete-windows-on "*compilation*")))

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
(define-key mickey-minor-mode-map (kbd "C-x M-d") 'duplicate)

(defun emacs-process-p (pid)
  "If pid is the process ID of an emacs process, return t, else nil.
Also returns nil if pid is nil."
  (when pid
    (let ((attributes (process-attributes pid)) (cmd))
      (dolist (attr attributes)
        (if (string= "comm" (car attr))
            (setq cmd (cdr attr))))
      (if (and cmd (or (string= "emacs" cmd) (string= "emacs.exe" cmd))) t))))

(defun escape-region (&optional regex to-string)
  "Escape double-quotes and backslashes.
You can control the regex replacement with the two optional
parameters."
  (interactive)
  (unless regex (setq regex "\\([\"\\\\]\\)"))
  (unless to-string (setq to-string "\\\\\\1"))
  (save-excursion
    (while (re-search-forward
            regex
            (if (not mark-active)
                (point-max)
              (when (> (point) (mark)) (exchange-point-and-mark))
              (mark))
            t)
      (replace-match to-string))))

(defun eval-and-replace ()
  "Replace the last sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun find-symbol-at-point ()
  "Find directly the symbol at point, i.e. go to definition."
  (interactive)
  (let ((sym (symbol-at-point)))
    (if (boundp sym)
        (find-variable sym)
      (find-function sym))))

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
;; above comment.
;; DONE: Fix reported to http://debbugs.gnu.org/cgi/bugreport.cgi?bug=20663.
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
If FILE is unspecified, look for 'Makefile'. If it does not find
FILE, then it shall return the name of FILE in the current
directory, suitable for creation. This may not do the correct
thing in presence of links."
  (let ((current-dir default-directory) (looping t) (makefile (or file "Makefile")))
    (while (progn
             (if (file-exists-p (expand-file-name makefile current-dir))
                 (setq looping nil)
               (setq current-dir (expand-file-name ".." current-dir)))
             (and looping (not (equal current-dir "/")))))
    (if (equal current-dir "/") nil (expand-file-name makefile current-dir))))

(defun insert-and-indent (text)
  "Insert indented TEXT at point."
  (interactive "s Text: ")
  (let ((oldpoint  (point)))
    (insert text)
    (indent-region oldpoint (point))
    (newline-and-indent)))

(defun insert-file-name (filename &optional args)
  "Insert name of file FILENAME into buffer after point.
Prefixed with \\[universal-argument], expand the file name to its
fully canonicalized path. See `expand-file-name'.

Prefixed with \\[negative-argument], use relative path to file
name from current directory, `default-directory'. See
`file-relative-name'.

The default with no prefix is to insert the file name exactly as
it appears in the minibuffer prompt."
  ;; Based on insert-file in Emacs -- ashawley 20080926
  (interactive "*fInsert file name: \nP")
  (cond ((eq '- args)
         (insert (file-relative-name filename)))
        ((not (null args))
         (insert (expand-file-name filename)))
        (t
         (insert filename))))
; (define-key mickey-minor-mode-map "\C-x\M-f" 'insert-file-name)

(defun insert-symbol-at-point-in-regexp-search-ring ()
  "Insert symbol at point in regexp search ring."
  (interactive)
  (add-to-history 'regexp-search-ring (find-tag-default-as-symbol-regexp)))
(define-key mickey-minor-mode-map "\M-#" 'insert-symbol-at-point-in-regexp-search-ring)

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

(defun load-external (ext feature &optional mode default)
  "Add the EXT regex to `auto-mode-alist' such that it loads the
associated symbol FEATURE. If FEATURE has not the same name as
the mode, you should provide the real mode name in symbol MODE.
If MODE is nil or unspecified, FEATURE is used as the mode name.
We call `autoload' to make the mode accessible interactively. We
need `require' to check if feature is loadable. It allows us to
fallback to the mode provided in symbol DEFAULT."
  (let ((local-mode (if mode mode feature)))
    (autoload local-mode (symbol-name feature) nil t)
    (add-to-list
     'auto-mode-alist
     (cons ext `(lambda ()
                  (if (require ',feature nil t)
                      (,local-mode)
                    ,(if (null default)
                         `(warn "Could not load %s, fallback to %s"
                                (symbol-name ',feature) (symbol-name ',default-major-mode))
                       `(progn
                          (,default)
                          (warn "Could not load %s, fallback to %s"
                                (symbol-name ',feature) (symbol-name ',default))))))))))

(defun mark-word-from-beginning (&optional arg allow-extend)
  "Set the point at the beginning of the word and call `mark-word'."
  (interactive "P\np")
  (cond ((and allow-extend
              (or (and (eq last-command this-command) (mark t))
                  (region-active-p)))
         (mark-word arg allow-extend))
        (t
         ;; The next line makes sure the word at point gets selected if point is
         ;; on the first letter. We need to ignore error if point is at EOF.
         (ignore-errors (forward-char))
         (backward-word)
         (mark-word arg allow-extend))))
(define-key mickey-minor-mode-map (kbd "M-@") 'mark-word-from-beginning)

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
(define-key mickey-minor-mode-map (kbd "M-(") 'move-border-left)

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
(define-key mickey-minor-mode-map (kbd "M-)") 'move-border-right)

(defun page-count ()
  "Return page count."
  (save-restriction
    (widen)
    (save-excursion
      (+ 1 (how-many
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

(defun page-number-mode (activate)
  "Display of page number in mode line.
If ACTIVATE is non-nil, enable, disable otherwise. Interactively,
activate unless called with \\[universal-argument].\n\nThis adds
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
(define-key mickey-minor-mode-map (kbd "C-x w") 'rename-buffer-and-file)

(defun sanitize ()
  "(Un)tabifies according to `indent-tabs-mode', indents and deletes trailing whitespace.
Works on buffer or region. Requires `tabify-leading'."
  (interactive)
  (save-excursion
    (unless mark-active
      (mark-whole-buffer))
    (if indent-tabs-mode
        (tabify-leading)
      (untabify (region-beginning) (region-end)))
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
(define-key mickey-minor-mode-map (kbd "C-M-!") 'shell-last-command)

(defun skeleton-make-markers ()
  "Save last skeleton markers in a list.
Hook function for skeletons."
  (while skeleton-markers
    (set-marker (pop skeleton-markers) nil))
  (setq skeleton-markers
        (mapcar 'copy-marker (reverse skeleton-positions))))

(defvar skeleton-markers nil
  "Markers for locations saved in skeleton-positions.")

(defun skeleton-next-position (&optional reverse)
  "Skeleton movements through placeholders."
  (interactive "P")
  (let ((positions (mapcar 'marker-position skeleton-markers))
        (comp (if reverse '< '<=))
        pos prev)
    (when positions
      (setq pos (pop positions))
      (while (and pos (funcall comp pos (point)))
        (setq prev pos)
        (setq pos (pop positions)))
      (cond
       ((and reverse prev) (goto-char prev))
       (reverse (goto-char (car (last skeleton-markers))))
       (pos (goto-char pos))
       (t (goto-char (car skeleton-markers)))))))

(defun sort-lines-unique (arg)
  "Remove trailing white space, then duplicate lines, then sort the result.
Do not fold case with \\[universal-argument] or non-nil ARG."
  (interactive "P")
  (unless mark-active
    (mark-whole-buffer))
  (when (> (point) (mark))
    (exchange-point-and-mark))
  (delete-trailing-whitespace (point) (mark))
  (delete-duplicate-lines (point) (mark))
  (let ((sort-fold-case (if arg nil t)))
    (sort-lines nil (point) (mark))))

(defun swap-windows ()
  "If 2 windows are up, swap them."
  (interactive)
  (unless (= 2 (count-windows))
    (error "There are not 2 windows"))
  (let* ((w1 (car (window-list)))
         (w2 (nth 1 (window-list)))
         (b1 (window-buffer w1))
         (b2 (window-buffer w2))
         (s1 (window-start w1))
         (s2 (window-start w2)))
    (set-window-buffer w1 b2)
    (set-window-buffer w2 b1)
    (set-window-start w1 s2)
    (set-window-start w2 s1))
  (other-window 1))
(define-key mickey-minor-mode-map (kbd "C-x \\") 'swap-windows)

(defun tabify-leading ()
  "Call `tabify' on leading spaces only.
Works on whole buffer if region is unactive."
  (interactive)
  (require 'tabify) ; Need this to initialize `tabify-regexp'.
  (let ((tabify-regexp-old tabify-regexp) start end)
    (if mark-active
        (setq start (region-beginning) end (region-end))
      (setq start (point-min) end (point-max)))
    (unwind-protect
        (progn
          (setq tabify-regexp "^\t* [ \t]+")
          (tabify start end))
      (setq tabify-regexp tabify-regexp-old))))

(defun toggle-indent-tabs ()
  "Indent with tabs or spaces."
  (interactive)
  (if indent-tabs-mode
      (progn
        (message "Indent using spaces")
        (setq indent-tabs-mode nil))
    (message "Indent using tabs")
    (setq indent-tabs-mode t)))
; (define-key mickey-minor-mode-map (kbd "C-x M-i") 'toggle-indent-tabs)

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
(define-key mickey-minor-mode-map [pause] 'toggle-window-dedicated)

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
(define-key mickey-minor-mode-map (kbd "C-x C-\\") 'toggle-window-split)

(defun toggle-word-delim ()
  "Make underscore part of the word syntax or not.
This does not interfere with `subword-mode'."
  (interactive)
  (if (equal (char-syntax ?_) "_")
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

(provide 'functions)
