;;; Python

(defun ambrevar/python-set-compiler ()
  "Returns the value of the shebang if any, `python-shell-interpreter' otherwise."
  (when buffer-file-name
    (let* ((firstline
            (save-excursion (goto-char (point-min)) (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
           (interpreter
            (if (not (string-match "^#!" firstline))
                python-shell-interpreter
              (substring firstline 2))))
      (setq compile-command
            (concat interpreter " " (shell-quote-argument buffer-file-name))))))

(add-hook 'python-mode-hook 'ambrevar/python-set-compiler)

;;; Doc lookup. Requires the python.info file to be installed. See
;;; https://bitbucket.org/jonwaltman/pydoc-info/.
;; (add-to-list 'load-path "~/path/to/pydoc-info")
;; (require 'pydoc-info nil t)

(provide 'init-python)
