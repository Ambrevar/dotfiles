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

(provide 'patch-eshell)
