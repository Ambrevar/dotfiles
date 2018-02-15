;;; Eshell patches.

;;; Fix 27407, expected in Emacs 26.1.
;;; http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=4ebdcc46ab345849332332d580bd1e3c2c9adb1e
;;; http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=37cdfec7c73205668498da9b27387f5f3ccfebee
;;; Test:
;;;
;; (insert "echo \"\e[33mhello\e[0m\"")
;; (insert "echo \"\e[33mhello\"")

(with-eval-after-load 'ansi-color
  (defun ansi-color-make-extent (from to &optional object)
    "Make an extent for the range [FROM, TO) in OBJECT.

OBJECT defaults to the current buffer.  XEmacs uses `make-extent', Emacs
uses `make-overlay'.  XEmacs can use a buffer or a string for OBJECT,
Emacs requires OBJECT to be a buffer."
    (if (fboundp 'make-extent)
        (make-extent from to object)
      ;; In Emacs, the overlay might end at the process-mark in comint
      ;; buffers.  In that case, new text will be inserted before the
      ;; process-mark, ie. inside the overlay (using insert-before-marks).
      ;; In order to avoid this, we use the `insert-behind-hooks' overlay
      ;; property to make sure it works.
      (let ((overlay (make-overlay from to object)))
        (overlay-put overlay 'modification-hooks '(ansi-color-freeze-overlay))
        (overlay-put overlay 'insert-behind-hooks '(ansi-color-freeze-overlay))
        overlay))))

(with-eval-after-load "esh-mode"
  (defun eshell-output-filter (process string)
    "Send the output from PROCESS (STRING) to the interactive display.
This is done after all necessary filtering has been done."
    (let ((oprocbuf (if process (process-buffer process)
                      (current-buffer)))
          (inhibit-point-motion-hooks t)
          (inhibit-modification-hooks t))
      (let ((functions eshell-preoutput-filter-functions))
        (while (and functions string)
          (setq string (funcall (car functions) string))
          (setq functions (cdr functions))))
      (if (and string oprocbuf (buffer-name oprocbuf))
          (let (opoint obeg oend)
            (with-current-buffer oprocbuf
              (setq opoint (point))
              (setq obeg (point-min))
              (setq oend (point-max))
              (let ((buffer-read-only nil)
                    (nchars (length string))
                    (ostart nil))
                (widen)
                (goto-char eshell-last-output-end)
                (setq ostart (point))
                (if (<= (point) opoint)
                    (setq opoint (+ opoint nchars)))
                (if (< (point) obeg)
                    (setq obeg (+ obeg nchars)))
                (if (<= (point) oend)
                    (setq oend (+ oend nchars)))
                ;; Let the ansi-color overlay hooks run.
                (let ((inhibit-modification-hooks nil))
                  (insert-before-markers string))
                (if (= (window-start) (point))
                    (set-window-start (selected-window)
                                      (- (point) nchars)))
                (if (= (point) eshell-last-input-end)
                    (set-marker eshell-last-input-end
                                (- eshell-last-input-end nchars)))
                (set-marker eshell-last-output-start ostart)
                (set-marker eshell-last-output-end (point))
                (force-mode-line-update))
              (narrow-to-region obeg oend)
              (goto-char opoint)
              (eshell-run-output-filters)))))))

(with-eval-after-load 'em-prompt
  (defun eshell-emit-prompt ()
    "Emit a prompt if eshell is being used interactively."
    (when (boundp 'ansi-color-context-region)
      (setq ansi-color-context-region nil))
    (run-hooks 'eshell-before-prompt-hook)
    (if (not eshell-prompt-function)
        (set-marker eshell-last-output-end (point))
      (let ((prompt (funcall eshell-prompt-function)))
        (and eshell-highlight-prompt
             (add-text-properties 0 (length prompt)
                                  '(read-only t
                                              font-lock-face eshell-prompt
                                              front-sticky (font-lock-face read-only)
                                              rear-nonsticky (font-lock-face read-only))
                                  prompt))
        (eshell-interactive-print prompt)))
    (run-hooks 'eshell-after-prompt-hook)))

;;; Fix 27405, expected in Emacs 26.1.
;;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=27405
;;; Emacs' standard functions fail when output has empty lines.
;;; The following implementation is more reliable.
(with-eval-after-load 'em-prompt
  (defun eshell-next-prompt (n)
    "Move to end of Nth next prompt in the buffer.
See `eshell-prompt-regexp'."
    (interactive "p")
    (re-search-forward eshell-prompt-regexp nil t n)
    (when eshell-highlight-prompt
      (while (not (get-text-property (line-beginning-position) 'read-only) )
        (re-search-forward eshell-prompt-regexp nil t n)))
    (eshell-skip-prompt))

  (defun eshell-previous-prompt (n)
    "Move to end of Nth previous prompt in the buffer.
See `eshell-prompt-regexp'."
    (interactive "p")
    (backward-char)
    (eshell-next-prompt (- n))))

;;; Fix bug#29157: 25.3; Eshell parsing fails sometimes [...]
(with-eval-after-load 'em-hist
  (defun eshell-hist-initialize ()
    "Initialize the history management code for one Eshell buffer."
    ;; PATCH: Disable the history references.
    ;; (add-hook 'eshell-expand-input-functions
    ;; 'eshell-expand-history-references nil t)

    (when (eshell-using-module 'eshell-cmpl)
      (add-hook 'pcomplete-try-first-hook
                'eshell-complete-history-reference nil t))

    (if (and (eshell-using-module 'eshell-rebind)
             (not eshell-non-interactive-p))
        (let ((rebind-alist eshell-rebind-keys-alist))
          (make-local-variable 'eshell-rebind-keys-alist)
          (setq eshell-rebind-keys-alist
                (append rebind-alist eshell-hist-rebind-keys-alist))
          (set (make-local-variable 'search-invisible) t)
          (set (make-local-variable 'search-exit-option) t)
          (add-hook 'isearch-mode-hook
                    (function
                     (lambda ()
                       (if (>= (point) eshell-last-output-end)
                           (setq overriding-terminal-local-map
                                 eshell-isearch-map)))) nil t)
          (add-hook 'isearch-mode-end-hook
                    (function
                     (lambda ()
                       (setq overriding-terminal-local-map nil))) nil t))
      (define-key eshell-mode-map [up] 'eshell-previous-matching-input-from-input)
      (define-key eshell-mode-map [down] 'eshell-next-matching-input-from-input)
      (define-key eshell-mode-map [(control up)] 'eshell-previous-input)
      (define-key eshell-mode-map [(control down)] 'eshell-next-input)
      (define-key eshell-mode-map [(meta ?r)] 'eshell-previous-matching-input)
      (define-key eshell-mode-map [(meta ?s)] 'eshell-next-matching-input)
      (define-key eshell-command-map [(meta ?r)]
        'eshell-previous-matching-input-from-input)
      (define-key eshell-command-map [(meta ?s)]
        'eshell-next-matching-input-from-input)
      (if eshell-hist-match-partial
          (progn
            (define-key eshell-mode-map [(meta ?p)]
              'eshell-previous-matching-input-from-input)
            (define-key eshell-mode-map [(meta ?n)]
              'eshell-next-matching-input-from-input)
            (define-key eshell-command-map [(meta ?p)] 'eshell-previous-input)
            (define-key eshell-command-map [(meta ?n)] 'eshell-next-input))
        (define-key eshell-mode-map [(meta ?p)] 'eshell-previous-input)
        (define-key eshell-mode-map [(meta ?n)] 'eshell-next-input)
        (define-key eshell-command-map [(meta ?p)]
          'eshell-previous-matching-input-from-input)
        (define-key eshell-command-map [(meta ?n)]
          'eshell-next-matching-input-from-input)))

    (make-local-variable 'eshell-history-size)
    (or eshell-history-size
        (let ((hsize (getenv "HISTSIZE")))
          (setq eshell-history-size
                (if (and (stringp hsize)
                         (integerp (setq hsize (string-to-number hsize)))
                         (> hsize 0))
                    hsize
                  128))))

    (make-local-variable 'eshell-history-file-name)
    (or eshell-history-file-name
        (setq eshell-history-file-name (getenv "HISTFILE")))

    (make-local-variable 'eshell-history-index)
    (make-local-variable 'eshell-save-history-index)

    (if (minibuffer-window-active-p (selected-window))
        (set (make-local-variable 'eshell-save-history-on-exit) nil)
      (set (make-local-variable 'eshell-history-ring) nil)
      (if eshell-history-file-name
          (eshell-read-history nil t))

      (add-hook 'eshell-exit-hook 'eshell-write-history nil t))

    (unless eshell-history-ring
      (setq eshell-history-ring (make-ring eshell-history-size)))

    (add-hook 'eshell-exit-hook 'eshell-write-history nil t)

    (add-hook 'kill-emacs-hook 'eshell-save-some-history)

    (make-local-variable 'eshell-input-filter-functions)
    (add-hook 'eshell-input-filter-functions 'eshell-add-to-history nil t)

    (define-key eshell-command-map [(control ?l)] 'eshell-list-history)
    (define-key eshell-command-map [(control ?x)] 'eshell-get-next-from-history)))

;;; Fix 29854, expected in Emacs 26.1?
(setq
 ansi-color-apply-face-function
 (lambda (beg end face)
   (when face
     (put-text-property beg end 'face face))))

;;; REVIEW: Ignore dups in the entire ring, not just the last entry.
;;; Reported upstream, see #30466.
(defun eshell-add-input-to-history (input)
  "Add the string INPUT to the history ring.
Input is entered into the input history ring, if the value of
variable `eshell-input-filter' returns non-nil when called on the
input."
  (when (funcall eshell-input-filter input)
    (when eshell-hist-ignoredups
      (ring-remove eshell-history-ring
                   (ring-member eshell-history-ring input)))
    (eshell-put-history input))
  (setq eshell-save-history-index eshell-history-index)
  (setq eshell-history-index nil))

(provide 'patch-eshell)
