;;==============================================================================
;; Shell
;;==============================================================================

;; Allow comment indentation.
(setq sh-indent-comment t)

(setq sh-shell-file "/bin/sh")

(defun shell-set-interpreter ()
  "If a shabang is present, use it as the shell interpreter,
otherwise use 'sh-shell-file'."
  "Use compile to run python programs."
  (interactive)
  (hack-local-variables)
  (let ((firstline
         (car
          (split-string (buffer-substring-no-properties 1 (point-max)) "\n"))))
    (let ((sh-interpreter
           (if (not (string-match "^#!" firstline))
               sh-shell-file
             (substring firstline 2))))

      (set (make-local-variable 'compile-command)
         (concat sh-interpreter " " buffer-file-name)))))

(add-hook
 'sh-mode-hook
 (lambda ()
   (shell-set-interpreter)))

(provide 'mode-shell)
