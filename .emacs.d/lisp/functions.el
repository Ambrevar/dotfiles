;;; Functions

;;; Notes on mark and region: to get a consistent behaviour regardless of
;;; Transient mode, check `(use-region-p)'. It will work as expected if
;;; transient. If not, it will always be true as soon as the mark has been set
;;; once; so you need to make sure the mark is set as you want beforehand (e.g.
;;; whole buffer, single line...). This is the behaviour of `sort-lines'.
;;;
;;; The clean way to get static region boundaries and fallback on buffer boundaries:
;;
;; (let (start end)
;;   (if (use-region-p)
;;       (setq start (region-beginning) end (region-end))
;;     (setq start (point-min) end (point-max)))
;;
;;; If several commands act on region and the region size/pos is susceptible to change:
;;
;; (let ((start (set-marker (make-marker) (if (use-region-p) (region-beginning) (point-min))))
;;       (end (set-marker (make-marker) (if (use-region-p) (region-end) (point-end)))))
;;
;;; For commands that only work on regions:
;;
;; (defun count-lines-region (start end)
;;   "Print number of lines and characters in the region."
;;   (interactive "r")
;;   ...

(defun ambrevar/call-process-to-string (program &rest args)
  "Call PROGRAM with ARGS and return output."
  (with-output-to-string
    (with-current-buffer standard-output
      (apply 'call-process program nil t nil args))))

(defun ambrevar/define-keys (map key def &rest bindings)
  "Like `define-key' but allow for defining several bindings at once.
`KEY' must be acceptable for `kbd'."
  (while key
    (define-key map (kbd key) def)
    (setq key (pop bindings)
          def (pop bindings))))

(defun ambrevar/escape-region (&optional regex to-string)
  "Escape double-quotes and backslashes.
This is useful for writing Elisp strings containing those
characters. The optional parameters let you control the replacement of REGEX for
TO-STRING."
  (interactive)
  (unless regex (setq regex "\\([\"\\\\]\\)"))
  (unless to-string (setq to-string "\\\\\\1"))
  (while (re-search-forward regex (if (use-region-p) (region-end) (point-max)) t)
    (replace-match to-string)))

(defun ambrevar/prettify ()
  "(Un)tabify, indent and delete trailing whitespace.

Tabify if `indent-tabs-mode' is true, otherwise use spaces.
Work on buffer or region.

If `ambrevar/prettify-inhibit-p' is non-nil, it does nothing.

Require `ambrevar/tabify-leading'."
  (interactive)
  (unless ambrevar/prettify-inhibit-p
    (let ((start (set-marker (make-marker) (if (use-region-p) (region-beginning) (point-min))))
          (end (set-marker (make-marker) (if (use-region-p) (region-end) (point-max)))))
      (if indent-tabs-mode
          (ambrevar/tabify-leading)
        (untabify start end))
      (indent-region start end)
      (save-restriction
        (narrow-to-region start end)
        (delete-trailing-whitespace)))))

(defcustom ambrevar/prettify-inhibit-p t
  "Do not run `ambrevar/prettify' if non-nil.
As this is not friendly to foreign projects, `ambrevar/prettify' should be run
selectively."
  :safe 'booleanp)

(defun ambrevar/flyspell-and-whitespace-mode ()
  "Toggle `flyspell-mode' and `whitespace-mode'."
  (interactive)
  (if (derived-mode-p 'text-mode)
      (flyspell-mode)
    (if flyspell-mode (flyspell-mode 0) (flyspell-prog-mode)))
  (whitespace-mode 'toggle))
(global-set-key (kbd "<f9>") 'flyspell-and-whitespace-mode)

;;; From https://www.reddit.com/r/emacs/comments/70bn7v/what_do_you_have_emacs_show_when_it_starts_up/.
;;; Supply a random fortune cookie as the *scratch* message.
(defun ambrevar/fortune-scratch-message ()
  (interactive)
  (let ((fortune
         (when (executable-find "fortune")
           (with-temp-buffer
             (shell-command "fortune" t)
             (while (not (eobp))
               (insert ";; ")
               (forward-line))
             (delete-trailing-whitespace (point-min) (point-max))
             (concat (buffer-string) "\n")))))
    (if (called-interactively-p 'any)
        (insert fortune)
      fortune)))

(defun ambrevar/global-set-keys (key def &rest bindings)
  "Like `global-set-key' but allow for defining several bindings at once.
`KEY' must be acceptable for `kbd'."
  (while key
    (global-set-key (kbd key) def)
    (setq key (pop bindings)
          def (pop bindings))))

(defun ambrevar/insert-and-indent (text)
  "Insert indented TEXT at point."
  (interactive "s Text: ")
  (let ((oldpoint  (point)))
    (insert text)
    (indent-region oldpoint (point))
    (newline-and-indent)))

(defun ambrevar/local-set-keys (key def &rest bindings)
  "Like `local-set-key' but allow for defining several bindings at once.
`KEY' must be acceptable for `kbd'."
  (while key
    (local-set-key (kbd key) def)
    (setq key (pop bindings)
          def (pop bindings))))

(defun ambrevar/move-border-left (arg)
  "Move window border in a natural manner.
If this is a window with its right edge being the edge of the
screen, enlarge the window horizontally. If this is a window with
its left edge being the edge of the screen, shrink the window
horizontally. Otherwise, default to enlarging horizontally.\n
Enlarge/Shrink by ARG columns, or 5 if ARG is nil."
  (interactive "P")
  (if (= (count-windows) 2)
      (ambrevar/move-border-left-or-right arg t)))
(global-set-key (kbd "M-(") 'ambrevar/move-border-left)

(defun ambrevar/move-border-left-or-right (arg dir-left)
  "Wrapper around ‘move-border-left’ and ‘move-border-right’.
ARG is the number of columns to move.
If DIR-LEFT is t, then move left, otherwise move right."
  (interactive)
  (unless arg (setq arg 5))
  (let ((left-edge (= (car (window-edges)) 0)))
    (if (or
         (and left-edge dir-left)
         (and (not left-edge) (not dir-left)))
        (shrink-window arg t)
      (enlarge-window arg t))))

(defun ambrevar/move-border-right (arg)
  "See `move-border-left'."
  (interactive "P")
  (if (= (count-windows) 2)
      (ambrevar/move-border-left-or-right arg nil)))
(global-set-key (kbd "M-)") 'ambrevar/move-border-right)

(defun ambrevar/reset-fill-column ()
  "Reset `fill-column' to its default value."
  (setq fill-column (default-value 'fill-column)))

(defun ambrevar/shell-last-command ()
  "Run last shell command."
  (interactive)
  (let ((last (car shell-command-history)))
    (if last
        (shell-command last)
      (error "Shell command history is empty"))))
(global-set-key (kbd "C-M-!") 'shell-last-command)

(defun ambrevar/skeleton-make-markers ()
  "Save last skeleton markers in a list.
Hook function for skeletons."
  (while skeleton-markers
    (set-marker (pop skeleton-markers) nil))
  (setq skeleton-markers
        (mapcar 'copy-marker (reverse skeleton-positions))))

(defvar skeleton-markers nil
  "Markers for locations saved in `skeleton-positions'.")

(defun ambrevar/skeleton-previous-position ()
  "Move to previous skeleton placeholder.
See `skeleton-next-position'."
  (skeleton-next-position t))

(defun ambrevar/skeleton-next-position (&optional reverse)
  "Move to next skeleton placeholder.
If REVERSE it t, move to previous placeholder."
  (interactive "P")
  (let ((positions (mapcar 'marker-position skeleton-markers))
        (comp (if reverse '< '<=))
        pos
        prev)
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

(defun ambrevar/sort-lines-unique (arg)
  "Remove trailing white space, then duplicate lines, then sort the result.
Do not fold case with \\[universal-argument] or non-nil ARG."
  (interactive "P")
  (let ((start (set-marker (make-marker) (if (use-region-p) (region-beginning) (point-min))))
        (end (set-marker (make-marker) (if (use-region-p) (region-end) (point-end)))))
    (let ((sort-fold-case (if arg nil t)))
      (delete-trailing-whitespace start end)
      (delete-duplicate-lines start end)
      (sort-lines nil start end))))

(defun ambrevar/swap-windows (&optional w1 w2)
  "If 2 windows are up, swap them.
Else if W1 is a window, swap it with current window.
If W2 is a window too, swap both."
  (interactive)
  (unless (or (= 2 (count-windows))
              (windowp w1)
              (windowp w2))
    (error "Ambiguous window selection"))
  (let* ((w1 (or w1 (car (window-list))))
         (w2 (or w2
                 (if (eq w1 (car (window-list)))
                     (nth 1 (window-list))
                   (car (window-list)))))
         (b1 (window-buffer w1))
         (b2 (window-buffer w2))
         (s1 (window-start w1))
         (s2 (window-start w2)))
    (with-temp-buffer
      ;; Some buffers like EXWM buffers can only be in one live buffer at once.
      ;; Switch to a dummy buffer in w2 so that we don't display any buffer twice.
      (set-window-buffer w2 (current-buffer))
      (set-window-buffer w1 b2)
      (set-window-buffer w2 b1))
    (set-window-start w1 s2)
    (set-window-start w2 s1))
  (select-window w1))
(global-set-key (kbd "C-x \\") 'swap-windows)

(defun ambrevar/swap-windows-left ()
  "Swap current window with the window to the left."
  (interactive)
  (swap-windows (window-in-direction 'left)))
(defun ambrevar/swap-windows-below ()
  "Swap current window with the window below."
  (interactive)
  (swap-windows (window-in-direction 'below)))
(defun ambrevar/swap-windows-above ()
  "Swap current window with the window above."
  (interactive)
  (swap-windows (window-in-direction 'above)))
(defun ambrevar/swap-windows-right ()
  "Swap current window with the window to the right."
  (interactive)
  (swap-windows (window-in-direction 'right)))

(defun ambrevar/switch-to-last-buffer ()
  "Switch to last open buffer in current window."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun ambrevar/tabify-leading ()
  "Call `tabify' on leading spaces only.
Works on whole buffer if region is unactive."
  (interactive)
  (require 'tabify) ; Need this to initialize `tabify-regexp'.
  (let ((tabify-regexp-old tabify-regexp) start end)
    (if (use-region-p)
        (setq start (region-beginning) end (region-end))
      (setq start (point-min) end (point-max)))
    (unwind-protect
        (progn
          (setq tabify-regexp "^\t* [ \t]+")
          (tabify start end))
      (setq tabify-regexp tabify-regexp-old))))

;;; TODO: Store window configurations in a buffer-name-indexed alist? Not
;;; sure that would ever be useful.
(defvar single-window--last-configuration nil "Last window configuration before calling `delete-other-windows'.")
(defun ambrevar/toggle-single-window ()
  "Un-maximize current window.
If multiple windows are active, save window configuration and
delete other windows.  If only one window is active and a window
configuration was previously save, restore that configuration."
  (interactive)
  (if (= (count-windows) 1)
      (when single-window--last-configuration
        (set-window-configuration single-window--last-configuration))
    (setq single-window--last-configuration (current-window-configuration))
    (delete-other-windows)))

(defun ambrevar/toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not.
Run it in each window you want to 'freeze', i.e. prevent Emacs
from acting on it."
  (interactive)
  (message
   (if (let ((window (get-buffer-window (current-buffer))))
         (set-window-dedicated-p window
                                 (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))
(global-set-key (kbd "<pause>") 'toggle-window-dedicated)

(defun ambrevar/toggle-window-split ()
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
(global-set-key (kbd "C-x C-\\") 'toggle-window-split)

(defun ambrevar/toggle-word-delim ()
  "Make underscore part of the word syntax or not.
This does not interfere with `subword-mode'."
  (interactive)
  (if (equal (char-syntax ?_) "_")
      (progn
        (modify-syntax-entry ?_ "w")
        (message "_ is a not word delimiter"))
    (modify-syntax-entry ?_ "_")
    (message "_ is a word delimiter")))

;;; TODO: Move turn-on-* functions to 'hook-functions.el'?
;;; And replace useless individual comments with a single global comment.

(defun ambrevar/turn-on-column-number-mode ()
  "Unconditionally turn on `column-number-mode' for the current buffer."
  (set (make-variable-buffer-local 'column-number-mode) t))

(defun ambrevar/turn-on-complete-filename ()
  "Unconditionally turn on `comint-dynamic-complete-filename' for the current buffer."
  (add-to-list 'completion-at-point-functions 'comint-dynamic-complete-filename t))

(defun ambrevar/turn-on-delete-trailing-whitespace ()
  "Add the `delete-trailing-whitespace' function to `before-save-hook'.
This does not affect .csv files."
  (unless (string= (file-name-extension buffer-file-name) "csv")
    (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)))

(defun ambrevar/turn-off-delete-trailing-whitespace ()
  "Unconditionally remove the `delete-trailing-whitespace' function to `before-save-hook'."
  (remove-hook 'before-save-hook 'delete-trailing-whitespace t))

(defun ambrevar/turn-on-prettify-before-save ()
  "Unconditionally add the `ambrevar/prettify' function to `before-save-hook'."
  (add-hook 'before-save-hook 'ambrevar/prettify nil t))

(defun ambrevar/turn-off-prettify-before-save ()
  "Unconditionally remove the `ambrevar/prettify' function to `before-save-hook'."
  (remove-hook 'before-save-hook 'ambrevar/prettify t))

(defun ambrevar/turn-off-indent-tabs ()
  "Unconditionally turn off tab indentation."
  (setq indent-tabs-mode nil))

(defun ambrevar/turn-on-indent-tabs ()
  "Unconditionally turn on tab indentation."
  (setq indent-tabs-mode t))

(defun ambrevar/turn-off-line-number-mode ()
  "Unconditionally turn off `line-number-mode' fur the current buffer.."
  (set (make-variable-buffer-local 'line-number-mode) nil))

(defun ambrevar/turn-off-linum ()
  "Unconditionally turn off Linum mode."
  (linum-mode 0))

(defun ambrevar/turn-on-newline-paragraph ()
  "Unconditionally make of newlines the start of a paragraph."
  (set (make-local-variable 'paragraph-start) "
"))

(defun ambrevar/turn-off-nobreak-char-display ()
  (set (make-local-variable 'nobreak-char-display) nil))

(defun ambrevar/turn-on-skeleton-markers ()
  "Allow skeletons to make markers to ease field navigation."
  (add-hook 'skeleton-end-hook 'skeleton-make-markers))

(defun ambrevar/turn-on-tab-width-to-4 ()
  "Unconditionally set tab width to 4."
  (setq tab-width 4))

(defun ambrevar/turn-on-tab-width-to-8 ()
  "Unconditionally set tab width to 8."
  (setq tab-width 8))

(defun ambrevar/unfill-paragraph ()
  "Paragraph at point is unwrapped on one single line."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun ambrevar/unfill-region ()
  "Unfill all paragraphs found in current region.
Each paragraph stand on its line."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-region (region-beginning) (region-end) nil)))

(provide 'functions)
