;;; Evil+term

(evil-set-initial-state 'term-mode 'insert)

;; TODO: Set prompt regexp.
;; TODO: Can prompt be read-only?

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
    (let (last-prompt last-bol)
      (save-excursion
        (goto-char (point-max))
        (when (= (line-beginning-position) (line-end-position))
          (ignore-errors (backward-char)))
        (setq last-prompt (term-bol nil)
              last-bol (line-beginning-position)))
      ;; We check if point is after both last-prompt and last-bol to handle multi-line prompts.
      (when (and (>= (point) last-prompt) (>= (point) last-bol))
        (term-char-mode)))))

(defun evil-term-setup ()
  (add-hook 'evil-insert-state-entry-hook 'evil-term-char-mode-entry-function)
  (add-hook 'evil-insert-state-exit-hook 'term-line-mode))
(add-hook 'term-mode-hook 'evil-term-setup)

;;; TODO: Report `term-char-mode' fix upstream.
;;; Originally, `term-char-mode' check if point is after `pmark' and if it is,
;;; it cuts the characters between pmark and point and sends them to the
;;; terminal.
;;; The idea is that when you write a commandline in char mode, then switch to
;;; line mode and keep on writing _without moving the point_, you can go back to
;;; char mode and keep the modifications.
;;; I'd say this is rather useless as the point of line mode is to _move
;;; around_, not to do the same thing you can do in char mode.
;;; The more sensical thing to do: erase process content, replace it with
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
    (let (last-prompt
          last-bol
          (pmark (process-mark (get-buffer-process (current-buffer)))))
      (save-excursion
        (goto-char (point-max))
        (when (= (line-beginning-position) (line-end-position))
          (ignore-errors (backward-char)))
        (setq last-prompt (term-bol nil)
              last-bol (line-beginning-position)))
      ;; Move char-mode point to line-mode point.
      ;; We check if point is after both last-prompt and last-bol to handle multi-line prompts.
      (when (and
             (>= (point) last-prompt)
             (>= (point) last-bol)
             (/= (point) pmark))
        (let ((term-move (if (> (point) pmark) 'term-send-right 'term-send-left)))
          (dotimes (_ (abs (- (point) pmark)))
            (funcall term-move)))))
    (term-update-mode-line)))

(provide 'init-evil-term)
