;;==============================================================================
;; Perl
;;==============================================================================

(add-hook
 'python-mode-hook
 (lambda ()
   (set (make-local-variable 'compilation-scroll-output) t)
   (set (make-local-variable 'compile-command) (concat "perl " buffer-file-name))))

(provide 'mode-perl)
