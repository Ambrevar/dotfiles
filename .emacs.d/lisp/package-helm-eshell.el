;;; TODO: Commit upstream.
;;; TODO: defvar for sources
;;; TODO: Support multiline prompts?
;;; TODO: Use candidate transformer and factor list function.
;;; TODO: Add action to yank prompt, output (prompt+output?)
;;; TODO: Reverse output.

(defface helm-eshell-prompts-promptno
  '((t (:foreground "cyan")))
  "Face used to highlight Eshell prompt index."
  :group 'helm-eshell-faces)

(defface helm-eshell-prompts-buffer-name
  '((t (:foreground "green")))
  "Face used to highlight Eshell buffer name."
  :group 'helm-eshell-faces)

(defcustom helm-eshell-prompts-max-length 400
  "Max number of chars displayed per candidate in the Eshell prompt browser.
When t, don't truncate candidate, show all.
By default it is approximatively the number of bytes contained in five lines
of 80 single-byte characters each i.e 80*5.
Note that if you set this to nil, multiline will be disabled, i.e. there
will not be separators between candidates anymore."
  :type '(choice (const :tag "Disabled" t)
                 (integer :tag "Max candidate offset"))
  :group 'helm-eshell)

(defcustom helm-eshell-prompts-promptno-p t
  "Show prompt number."
  :group 'helm-eshell
  :type 'boolean)

(defun helm-eshell-prompts-list ()
  "Return an alist of (prompt . pos)."
  (let (list)
    (save-excursion
      (goto-char (point-min))
      (let ((promptno 1))
        (while (not (eobp))
          (eshell-next-prompt 1)
          (let ((prompt (buffer-substring-no-properties (point) (line-end-position))))
            (setq list (cons `(,(if helm-eshell-prompts-promptno-p
                                    (format "%s:%s"
                                            (propertize (number-to-string promptno) 'face 'helm-eshell-prompts-promptno)
                                            prompt)
                                  prompt)
                               . ,(point))
                             list)
                  promptno (1+ promptno))))))
    list))

(defun helm-eshell-prompts-list-all ()
  "Return an alist of (\"buffer-name:prompt\" . (buffer . pos))."
  (let (list)
    (dolist (b (buffer-list))
      (with-current-buffer b
        (when (eq major-mode 'eshell-mode)
          (save-excursion
            (goto-char (point-min))
            (let ((buffer-name (propertize (buffer-name) 'face 'helm-eshell-prompts-buffer-name))
                  (promptno 1))
              (while (not (eobp))
                (eshell-next-prompt 1)
                (setq list (cons `(,(format "%s:%s:%s"
                                            buffer-name
                                            (propertize (number-to-string promptno) 'face 'helm-eshell-prompts-promptno)
                                            (buffer-substring-no-properties (point) (line-end-position)))
                                   . (,b . ,(point)))
                                 list))
                (setq promptno (1+ promptno))))))))
    list))

(defun helm-eshell-prompts-goto (candidate)
  (goto-char candidate))

;; TODO: Merge goto and goto-buffer, check for buffer existence in candidate.
(defun helm-eshell-prompts-goto-buffer (candidate)
  ;; (if (alist-get ...)...
  (switch-to-buffer (car candidate))
  (goto-char (cdr candidate)))

(defun helm-eshell-prompts-goto-buffer-other-window (candidate)
  (switch-to-buffer-other-window (car candidate))
  (goto-char (cdr candidate)))

(defun helm-eshell-prompts-goto-buffer-other-window (candidate)
  (switch-to-buffer-other-frame (car candidate))
  (goto-char (cdr candidate)))

(defvar helm-eshell-prompts-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c o") 'helm-eshell-goto-all-prompt-other-window)
    (define-key map (kbd "C-c C-o") 'helm-eshell-goto-all-prompt-other-frame)
    map)
  "Keymap for `helm-eshell-prompt-all'.")

;;;###autoload
(defun helm-eshell-prompts ()
  "Pre-configured `helm' to browse the prompts of the current Eshell."
  (interactive)
  (if (eq major-mode 'eshell-mode)
      (helm :sources
            (helm-build-sync-source "Eshell prompts"
              ;; :init (lambda ()
              ;; (helm-attrset 'multiline helm-eshell-prompts-max-length))
              :candidates (helm-eshell-prompts-list)
              :action '(("Go to prompt" . helm-eshell-prompts-goto))
              ;; :multiline 'helm-eshell-prompts-max-length)
              )
            :buffer "*helm-eshell-prompts*")
    (message "Current buffer is not an Eshell buffer")))

;;;###autoload
(defun helm-eshell-prompts-all ()
  "Pre-configured `helm' to browse the prompts of all Eshell sessions."
  (interactive)
  (helm :sources
        (helm-build-sync-source "All Eshell prompts"
          :candidates (helm-eshell-prompts-list-all)
          :action '(("Go to prompt" . helm-eshell-prompts-goto-buffer)
                    ("Go to prompt in other window `C-c o`" . helm-eshell-prompts-goto-buffer-other-window)
                    ("Go to prompt in other frame `C-c C-o`" . helm-eshell-prompts-goto-buffer-other-frame)))
        :buffer "*helm-eshell-all-prompts*"))

(add-to-list 'helm-source-names-using-follow "helm-eshell-prompts")

(provide 'package-helm-eshell)
