;;; TODO: Commit upstream.
;;; TODO: Support multiline prompts?
;;; TODO: Use candidate transformer and factor list function.
;;; TODO: Add yanking prompt, output (prompt+output?)
;;; TODO: Namespace: helm-eshell-prompt
;;; TODO: Reverse output.

(defface helm-eshell-promptno
  '((t (:foreground "cyan")))
  "Face used to highlight Eshell prompt index."
  :group 'helm-eshell-faces)

(defface helm-eshell-buffer-name
  '((t (:foreground "green")))
  "Face used to highlight Eshell buffer name."
  :group 'helm-eshell-faces)

(defcustom helm-eshell-prompt-show-no t
  "Show prompt number."
  :group 'helm-eshell
  :type 'boolean)

(defun helm-eshell-prompt-list ()
  "Return an alist of (prompt . pos)."
  (let (list)
    (save-excursion
      (beginning-of-buffer)
      (let ((promptno 1))
        (while (not (eobp))
          (eshell-next-prompt 1)
          (let ((prompt (buffer-substring-no-properties (point) (line-end-position))))
            (setq list (cons `(,(if helm-eshell-prompt-show-no
                                    (format "%s:%s"
                                            (propertize (number-to-string promptno) 'face 'helm-eshell-promptno)
                                            prompt)
                                  prompt)
                               . ,(point))
                             list)
                  promptno (1+ promptno))))))
    list))

(defun helm-eshell-all-prompt-list ()
  "Return an alist of (\"buffer-name:prompt\" . (buffer . pos))."
  (let (list)
    (dolist (b (buffer-list))
      (with-current-buffer b
        (when (eq major-mode 'eshell-mode)
          (save-excursion
            (beginning-of-buffer)
            (let ((buffer-name (propertize (buffer-name) 'face 'helm-eshell-buffer-name))
                  (promptno 1))
              (while (not (eobp))
                (eshell-next-prompt 1)
                (setq list (cons `(,(format "%s:%s:%s"
                                            buffer-name
                                            (propertize (number-to-string promptno) 'face 'helm-eshell-promptno)
                                            (buffer-substring-no-properties (point) (line-end-position)))
                                   . (,b . ,(point)))
                                 list))
                (setq promptno (1+ promptno))))))))
    list))

(defun helm-eshell-goto-prompt (candidate)
  (goto-char candidate))

(defun helm-eshell-goto-all-prompt (candidate)
  (switch-to-buffer (car candidate))
  (goto-char (cdr candidate)))

(defun helm-eshell-goto-all-prompt-other-window (candidate)
  (switch-to-buffer-other-window (car candidate))
  (goto-char (cdr candidate)))

(defun helm-eshell-goto-all-prompt-other-frame (candidate)
  (switch-to-buffer-other-frame (car candidate))
  (goto-char (cdr candidate)))

(defvar helm-eshell-goto-prompt-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c o") 'helm-eshell-goto-all-prompt-other-window)
    (define-key map (kbd "C-c C-o") 'helm-eshell-goto-all-prompt-other-frame)
    map)
  "Keymap for `helm-eshell-prompt-all'.")

(with-eval-after-load 'helm
  ;; TODO: Add autoloads
  (defun helm-eshell-prompts ()
    "Preconfigured `helm' to browse the prompts of the current Eshell."
    (interactive)
    (if (eq major-mode 'eshell-mode)
        (helm :sources
              (helm-build-sync-source "Eshell prompts"
                :candidates (helm-eshell-prompt-list)
                :action '(("Go to prompt" . helm-eshell-goto-prompt)))
              :buffer "*helm-eshell-prompts*")
      (message "Current buffer is not an Eshell buffer")))

  (defun helm-eshell-prompts-all ()
    "Preconfigured `helm' to browse the prompts of all Eshell sessions."
    (interactive)
    (helm :sources
          (helm-build-sync-source "All Eshell prompts"
            :candidates (helm-eshell-all-prompt-list)
            :action '(("Go to prompt" . helm-eshell-goto-all-prompt)
                      ("Go to prompt in other window `C-c o`" . helm-eshell-goto-all-prompt-other-window)
                      ("Go to prompt in other frame `C-c C-o`" . helm-eshell-goto-all-prompt-other-frame)
                      :))
          :buffer "*helm-eshell-all-prompts*"))

  (add-to-list 'helm-source-names-using-follow "helm-eshell-prompts"))

(provide 'package-helm-eshell)
