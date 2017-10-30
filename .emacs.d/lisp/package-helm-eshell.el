;;; TODO: Commit upstream.
;;; TODO: defvar for sources?
;;; TODO: Support multiline prompts? Probably hard since there are few means to know where output starts.

(defface helm-eshell-prompts-promptidx
  '((t (:foreground "cyan")))
  "Face used to highlight Eshell prompt index."
  :group 'helm-eshell-faces)

(defface helm-eshell-prompts-buffer-name
  '((t (:foreground "green")))
  "Face used to highlight Eshell buffer name."
  :group 'helm-eshell-faces)

(defcustom helm-eshell-prompts-promptidx-p t
  "Show prompt number."
  :group 'helm-eshell
  :type 'boolean)

(defun helm-eshell-prompts-list (&optional buffer)
  "List the prompts in Eshell BUFFER.

Return a list of (\"prompt\" (point) (buffer-name) prompt-index)).
If BUFFER is nil, use current buffer."
  (let ((buffer (or buffer (current-buffer)))
        list)
    (with-current-buffer buffer
      (when (eq major-mode 'eshell-mode)
        (save-excursion
          (goto-char (point-min))
          (let ((promptno 1))
            (while (not (eobp))
              (eshell-next-prompt 1)
              (setq list (cons (list (buffer-substring-no-properties (point) (line-end-position))
                                     (point)
                                     (buffer-name)
                                     (if helm-eshell-prompts-promptidx-p promptno))
                               list)
                    promptno (1+ promptno)))))))
    (nreverse list)))

(defun helm-eshell-prompts-list-all ()
  "List the prompts of all Eshell buffers.
See `helm-eshell-prompts-list'."
  (let (list)
    (dolist (b (buffer-list))
      (setq list (append (helm-eshell-prompts-list b) list)))
    list))

(defun helm-eshell-prompts-transformer (candidates &optional all)
  (dolist (c candidates)
    (setcar c
            (concat
             (when all
               (concat (propertize (nth 2 c) 'face 'helm-eshell-prompts-buffer-name) ":"))
             (when helm-eshell-prompts-promptidx-p
               (concat (propertize (number-to-string (nth 3 c)) 'face 'helm-eshell-prompts-promptidx) ":"))
             (car c))))
  candidates)

(defun helm-eshell-prompts-all-transformer (candidates &optional all)
  (helm-eshell-prompts-transformer candidates t))

(defun helm-eshell-prompts-goto (candidate)
  (when (nth 1 candidate)
    (switch-to-buffer (nth 1 candidate)))
  (goto-char (car candidate)))

(defun helm-eshell-prompts-goto-other-window (candidate)
  (switch-to-buffer-other-window (cdr candidate))
  (goto-char (car candidate)))

(defun helm-eshell-prompts-goto-other-frame (candidate)
  (switch-to-buffer-other-frame (cdr candidate))
  (goto-char (car candidate)))

(defvar helm-eshell-prompts-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c o") 'helm-eshell-prompts-goto-other-window)
    (define-key map (kbd "C-c C-o") 'helm-eshell-prompts-goto-other-frame)
    map)
  "Keymap for `helm-eshell-prompt-all'.")

;;;###autoload
(defun helm-eshell-prompts ()
  "Pre-configured `helm' to browse the prompts of the current Eshell."
  (interactive)
  (if (eq major-mode 'eshell-mode)
      (helm :sources
            (helm-build-sync-source "Eshell prompts"
              :candidates (helm-eshell-prompts-list)
              :candidate-transformer 'helm-eshell-prompts-transformer
              :action '(("Go to prompt" . helm-eshell-prompts-goto))
              )
            :buffer "*helm Eshell prompts*")
    (message "Current buffer is not an Eshell buffer")))

;;;###autoload
(defun helm-eshell-prompts-all ()
  "Pre-configured `helm' to browse the prompts of all Eshell sessions."
  (interactive)
  (helm :sources
        (helm-build-sync-source "All Eshell prompts"
          :candidates (helm-eshell-prompts-list-all)
          :candidate-transformer 'helm-eshell-prompts-all-transformer
          :action '(("Go to prompt" . helm-eshell-prompts-goto)
                    ("Go to prompt in other window `C-c o`" . helm-eshell-prompts-goto-other-window)
                    ("Go to prompt in other frame `C-c C-o`" . helm-eshell-prompts-goto-other-frame)))
        :buffer "*helm Eshell all prompts*"))

;;; Custom
(add-to-list 'helm-source-names-using-follow "Eshell prompts")
(add-to-list 'helm-source-names-using-follow "All Eshell prompts")

(provide 'package-helm-eshell)
