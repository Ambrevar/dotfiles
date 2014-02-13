;;==============================================================================
;; Perl
;;==============================================================================

(defun perl-set-interpreter ()
  "Use compile to run perl programs."
  (interactive)
  (set (make-local-variable 'compile-command)
       (concat "perl " buffer-file-name)))

(add-hook
 'python-mode-hook
 (lambda ()
   (set (make-local-variable 'compilation-scroll-output) t)
   (perl-set-interpreter)))

(provide 'mode-perl)
