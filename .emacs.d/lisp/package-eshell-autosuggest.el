(require 'company)

(defun company-eshell-autosuggest-candidates (prefix)
  (let* ((history
          (cl-remove-duplicates
           (mapcar (lambda (str)
                     (string-trim (substring-no-properties str)))
                   (ring-elements eshell-history-ring))
           :from-end t
           :test #'string=))
         (most-similar (cl-find-if
                        (lambda (str)
                          (string-prefix-p prefix str))
                        history)))
    (when most-similar
      `(,most-similar))))

(defun company-eshell-autosuggest--prefix ()
  (let ((prefix
         (string-trim-left
          (buffer-substring-no-properties
           (save-excursion
             (eshell-bol))
           (save-excursion (end-of-line) (point))))))
    (if (not (string-empty-p prefix))
        prefix
      'stop)))

(defun company-eshell-autosuggest (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-eshell))
    (prefix (and (eq major-mode 'eshell-mode)
                 (company-eshell-autosuggest--prefix)))
    (candidates (company-eshell-autosuggest-candidates arg))))

(defun eshell-setup-autosuggest ()
  (setq-local company-idle-delay 0.5)
  (setq-local company-backends '(company-eshell-autosuggest))
  (setq-local company-frontends '(company-preview-frontend)))

(provide 'package-eshell-autosuggest)
