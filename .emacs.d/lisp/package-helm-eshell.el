;;; TODO: Commit upstream.
;;; TODO: Global version?

(defun helm-eshell-prompt-list ()
  "Return an alist of (prompt . pos)."
  (let (list)
    (save-excursion
      (beginning-of-buffer)
      (while (not (eobp))
        (eshell-next-prompt 1)
        (setq list (cons `(,(buffer-substring-no-properties (point) (line-end-position)) . ,(point)) list))))
    list))

(defun helm-eshell-goto-prompt (candidate)
  (goto-char candidate))

(with-eval-after-load 'helm
  (defun helm-eshell-browse-prompts ()
    "Preconfigured `helm' to browse all prompts."
    (interactive)
    (helm :sources
          (helm-build-sync-source "Eshell prompts"
            :candidates (helm-eshell-prompt-list)
            :action '(("Go to prompt" . helm-eshell-goto-prompt)))
          :buffer "*helm-eshell-prompts*"))

  (add-to-list 'helm-source-names-using-follow "helm-eshell-prompts"))
