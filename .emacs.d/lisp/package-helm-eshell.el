;;; TODO: Commit upstream.
;;; TODO: Support multiline prompts?
;;; TODO: Add colored numbers and color shell names.

(defface helm-eshell-promptno
  '((t (:foreground "Darkorange1")))
  "Face used to highlight Eshell prompt index."
  :group 'helm-eshell-faces)

(defface helm-eshell-buffer-name
  '((t (:foreground "Green")))
  "Face used to highlight Eshell buffer name."
  :group 'helm-eshell-faces)

(defun helm-eshell-prompt-list ()
  "Return an alist of (prompt . pos)."
  (let (list)
    (save-excursion
      (beginning-of-buffer)
      (while (not (eobp))
        (eshell-next-prompt 1)
        (setq list (cons `(,(buffer-substring-no-properties (point) (line-end-position)) . ,(point)) list))))
    list))

(defun helm-eshell-all-prompt-list ()
  "Return an alist of (\"buffer-name:prompt\" . (buffer . pos))."
  (let (list)
    (dolist (b (buffer-list))
      (with-current-buffer b
        (when (eq major-mode 'eshell-mode)
          (save-excursion
            (beginning-of-buffer)
            (let ((buffer-name (buffer-name))
                  (count 1))
              (while (not (eobp))
                (eshell-next-prompt 1)
                (setq list (cons `(,(format "%s:%s:%s"
                                            buffer-name
                                            count
                                            (buffer-substring-no-properties (point) (line-end-position)))
                                   . (,b . ,(point)))
                                 list))
                (setq count (1+ count))))))))
    list))

(defun helm-eshell-goto-prompt (candidate)
  (goto-char candidate))

(defun helm-eshell-goto-all-prompt (candidate)
  (switch-to-buffer (car candidate))
  (goto-char (cdr candidate)))

(with-eval-after-load 'helm
  (defun helm-eshell-browse-prompts ()
    "Preconfigured `helm' to browse the prompts of the current Eshell."
    (interactive)
    (helm :sources
          (helm-build-sync-source "Eshell prompts"
            :candidates (helm-eshell-prompt-list)
            :action '(("Go to prompt" . helm-eshell-goto-prompt)))
          :buffer "*helm-eshell-prompts*"))

  (defun helm-eshell-browse-all-prompts ()
    "Preconfigured `helm' to browse the prompts of all Eshell sessions."
    (interactive)
    (helm :sources
          (helm-build-sync-source "Eshell all prompts"
            :candidates (helm-eshell-all-prompt-list)
            :action '(("Go to prompt" . helm-eshell-goto-all-prompt)))
          :buffer "*helm-eshell-all-prompts*"))

  (add-to-list 'helm-source-names-using-follow "helm-eshell-prompts"))

(provide 'package-helm-eshell)
