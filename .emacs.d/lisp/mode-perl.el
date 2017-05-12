;; Perl

(defvaralias 'perl-indent-level 'tab-width)

(add-hook-and-eval
 'perl-mode-hook
 (lambda ()
   (setq compile-command (concat "perl " (shell-quote-argument buffer-file-name)))))

(provide 'mode-perl)
