;; Python

(defun python-version ()
  "Returns \"python2\" or \"python3\" according to the shabang.
`python-shell-interpreter' is assumed by default."
  (let ((firstline
         (car
          (split-string (buffer-substring-no-properties 1 (point-max)) "\n"))))
    (if (not (string-match "^#!" firstline))
        "python"
      (cond
       ((string-match "python2" firstline) "python2")
       ((string-match "python3" firstline) "python3")
       (t python-shell-interpreter)))))

(defun python-set-interpreter ()
  "Use compile to run python programs."
  (interactive)
  (set (make-local-variable 'compile-command)
       (concat (python-version) " " (shell-quote-argument buffer-file-name))))

(add-hook-and-eval
 'python-mode-hook
 (lambda ()
   (set (make-local-variable 'compilation-scroll-output) t)
   (add-hook 'compilation-before-hook 'python-set-interpreter nil t)))

;; Doc lookup. Requires the python.info file to be installed. See
;; https://bitbucket.org/jonwaltman/pydoc-info/.
;; (add-to-list 'load-path "~/path/to/pydoc-info")
;; (require 'pydoc-info nil t)

(provide 'mode-python)
