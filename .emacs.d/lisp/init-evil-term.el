;;; Evil+term

(evil-set-initial-state 'term-mode 'insert)

;; TODO: Rebinding ESC has the drawback that programs like vi cannot use it anymore.
;; Workaround: switch to Emacs mode and double-press ESC.
;; Otherwise leave it ESC to C-cC-j.
;; Or bind char-mode ESC to C-cC-x?

;; TODO: Move this out of Evil? No, it depends on modal editing.
;; TODO: A new line gets inserted when calling M-b, M-f in char-mode
;; and then switching to line-mode. Seems like it happens on the first prompt
;; only.
(defun evil-term-char-mode-goto-point ()
  "Switch to char mode.
To switch to char mode, just swith to insert mode somewhere on the last line of the lat prompt."
  (interactive) ; No need for intertice if set in a hook.
  (when (get-buffer-process (current-buffer))
    ;; (term-char-mode)
    ;; (evil-insert-state)
    ;; TODO: When going from char->line mode, point is not necessarily at the end. To come back, Insert and delete char. `term-send-backspace'.
    ;; Can this break text-mode programs? Or anything reading input without waiting for EOL?
    ;; TODO: Even better: if point in line mode is after last prompt on last line, find it in char mode by call enough `term-send-left' and right.  Then no need for C-cC-k.
    ;; It's important that point must be on last line, because when point is on a multi-line command, it cannot go back to the previous lines.
    (let ((last-prompt (save-excursion (goto-char (point-max)) (when (= (line-beginning-position) (line-end-position)) (backward-char)) (term-bol nil)))
          (last-bol (save-excursion (goto-char (point-max)) (when (= (line-beginning-position) (line-end-position)) (backward-char)) (line-beginning-position)))
          (ref-point (point))
          ;; TODO: Refactor code without last-point if not be needed. Test when
          ;; background program outputs beyond the char-mode commandline.
          (last-point (point)))
      ;; TODO: Optimize by setting left/right func to var.
      (when (and (>= (point) last-prompt) (>= (point) last-bol))
        (term-char-mode)
        ;; Send a cursor move so that Emacs' point gets updated to the last
        ;; char-mode position, off-by-one.
        (term-send-left)
        ;; If moving left did not move the cursor, we are at term-bol, so move right.
        (when (= (point) last-point)
          (term-send-right))
        ;; If (point) is still last-point, it means there is no room for moving,
        ;; i.e. commandline is empty in char-mode.  We don't need to move any
        ;; further.
        ;; Otherwise, move to ref-point.
        (while (and (/= (point) last-point) (/= (point) ref-point))
          (setq last-point (point))
          (if (> (point) ref-point) (term-send-left) (term-send-right)))))
    ;; TODO: Add this to insert state entry hook and remove this line.
    (evil-insert-state)))

(defun evil-term-char-mode-insert ()
  (interactive)
  (when (term-in-line-mode)
    (term-char-mode))
  (unless (eq evil-state 'insert)
    (evil-insert-state)))

;; TODO: Test these.
(evil-define-key '(normal insert) term-mode-map
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

;; Bind something to it? C-cC-c is used by Eshell for that, but it is taken here, no? Keep C-cC-k.
;; (defun evil-term-char-mode-entry-function ()
;;   ;; Neet to check if we have a process, this is not the case when term-mode-hook is run.
;;   (when (and (= (point) (point-max)) (get-buffer-process (current-buffer)))
;;     (term-char-mode)))

(defun evil-term-setup ()
  ;; (add-hook 'evil-insert-state-entry-hook 'evil-term-char-mode-entry-function nil t)
  ;; (add-hook 'evil-insert-state-entry-hook 'evil-term-char-mode-goto-point nil t)
  (add-hook 'evil-insert-state-exit-hook 'term-line-mode nil t))
(add-hook 'term-mode-hook 'evil-term-setup)

(provide 'init-evil-term)
