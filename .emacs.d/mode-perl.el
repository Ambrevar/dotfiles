;;==============================================================================
;; Perl
;;==============================================================================

(defvaralias 'perl-indent-level 'tab-width)

(add-hook-and-eval
 'perl-mode-hook
 (lambda ()
   (set (make-local-variable 'compile-command) (concat "perl " buffer-file-name))))

(provide 'mode-perl)
