;;; Evil+term

(evil-set-initial-state 'term-mode 'insert)

;; TODO: Set prompt regexp. Test next/previous prompt functions, term-bol, etc.
;; Probably needs the same fix as Eshell.
;; TODO: Can the prompt be read-only?

;; TODO: Rebinding ESC has the drawback that programs like vi cannot use it anymore.
;; Workaround: switch to Emacs mode and double-press ESC.
;; Otherwise leave ESC to C-cC-j.
;; Or bind char-mode ESC to C-cC-x?

(defun evil-term-char-mode-insert ()
  (interactive)
  (term-char-mode)
  (evil-insert-state))

(evil-define-key 'normal term-mode-map
  "\C-c\C-k" 'evil-term-char-mode-insert
  (kbd "RET") 'term-send-input)

(evil-define-key 'insert term-mode-map "\C-c\C-k" 'term-char-mode)

(evil-define-key 'normal term-mode-map
  "[" 'term-previous-prompt
  "]" 'term-next-prompt
  "\C-k" 'eshell-previous-prompt
  "\C-j" 'eshell-next-prompt
  "\M-k" 'eshell-previous-prompt ; Custom
  "\M-j" 'eshell-next-prompt ; Custom
  ;; TODO: Why not J/K? Already bound to join/look-up. Does it matter?
  "0" 'term-bol
  "$" 'term-show-maximum-output)

(defun evil-term-char-mode-entry-function ()
  (when (get-buffer-process (current-buffer))
    (let (last-prompt)
      (save-excursion
        (goto-char (point-max))
        (when (= (line-beginning-position) (line-end-position))
          (ignore-errors (backward-char)))
        (setq last-prompt (max (term-bol nil) (line-beginning-position))))
      (when (>= (point) last-prompt)
        (term-char-mode)))))

(defun evil-term-setup ()
  (add-hook 'evil-insert-state-entry-hook 'evil-term-char-mode-entry-function)
  (add-hook 'evil-insert-state-exit-hook 'term-line-mode))
(add-hook 'term-mode-hook 'evil-term-setup)

;;; TODO: Report `term-char-mode' fix upstream.
;;;
;;; Originally, `term-char-mode' check if point is after `pmark' and if it is,
;;; it cuts the characters between pmark and point and sends them to the
;;; terminal.  The idea is that when you write a commandline in char mode, then
;;; switch to line mode and keep on writing _without moving the point_, you can
;;; go back to char mode and keep the modifications.
;;;
;;; I'd say this is rather useless as the point of line mode is to _move
;;; around_, not to do the same thing you can do in char mode.
;;;
;;; The more sensical thing to do: replace char-mode's commandline with
;;; line-mode's commandline and move char-mode point to where line-mode point
;;; is.
(defun term-char-mode ()
  "Switch to char (\"raw\") sub-mode of term mode.
Each character you type is sent directly to the inferior without
intervention from Emacs, except for the escape character (usually C-c)."
  (interactive)
  ;; FIXME: Emit message? Cfr ilisp-raw-message
  (when (term-in-line-mode)
    (setq term-old-mode-map (current-local-map))
    (use-local-map term-raw-map)
    (easy-menu-add term-terminal-menu)
    (easy-menu-add term-signals-menu)
    (let* (last-prompt
           commandline
           commandline-end-position
           (line-mode-point (point))
           (proc (get-buffer-process (current-buffer)))
           (pmark (process-mark proc)))
      (save-excursion
        (goto-char (point-max))
        (when (= (line-beginning-position) (line-end-position))
          ;; Sometimes a spurious newline gets inserted.
          ;; Work around it by skipping back past it.
          (ignore-errors (backward-char)))
        (setq
         ;; If the prompt regexp is wrong or if we are on a multiline prompt, get the line-beginning-position.
         last-prompt (max (term-bol nil) (line-beginning-position))
         ;; Yank last commandline.  If prompt is not properly recognized, it yanks the whole line.
         commandline (buffer-substring-no-properties last-prompt (line-end-position))
         ;; We store the end-position here so that we don't have to wait for the
         ;; process when we send the commandline.
         commandline-end-position (line-end-position)))

      (when (and (>= (point) last-prompt)
                 ;; commandline can be empty (e.g. term is still initializing),
                 ;; then no need to continue.
                 (not (string-empty-p commandline)))
        ;; Clear line.
        (dotimes (_ (abs (- last-prompt pmark)))
          (term-send-backspace))
        ;; Point could be before line-end-position, so we need to delete the trailing characters.
        (dotimes (_ (abs (- (point-max) pmark)))
          (term-send-del))
        ;; Get prompt position with `process-mark'.
        ;; TODO: We need to wait a bit to make sure the previously-sent deletions have been processed.
        ;; This is brittle and it makes the prompt flicker once.
        ;; It is possible to work-around this: either use `term-prompt-regexp'
        ;; or send `term-left' enough times, wait and get it.  That latter
        ;; solution has the advantage that it does not flickers, but it won't
        ;; work on dash.
        (sleep-for 0 100)
        (setq pmark (process-mark proc))
        ;; Remove actual prompt length from the commandline.
        (setq commandline (substring commandline (- pmark last-prompt)))
        ;; Send commandline to term.
        (term-send-raw-string commandline)

        ;; Move char-mode point to line-mode point. TODO: Don't do this if shell does not support cursor moves.
        ;; Underlying shell can be retrieved with:
        ;; (car (last (process-command (get-buffer-process (current-buffer)))))
        (dotimes (_ (abs (- line-mode-point commandline-end-position)))
          (term-send-left)))

      ;; Finish up.
      (term-update-mode-line))))

(provide 'init-evil-term)
