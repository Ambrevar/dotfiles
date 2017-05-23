;; Texinfo

;; We use the TeX setup.
(require 'tex-mode)
(require 'mode-tex)

(local-set-key (kbd "C-c C-b") 'texinfo-menu-update)

(defun texinfo-menu-update ()
  "Update texinfo node menu automatically."
  (interactive)
  (hack-local-variables)
  (texinfo-multiple-files-update (or tex-master-file buffer-file-name) t 8))

(add-hook-and-eval
 'texinfo-mode-hook
 (lambda ()
   (setq fill-column 80) ;; Is it needed?
   (set (make-local-variable 'tex-extension-list)
         '("aux" "cp" "cps" "fn" "ky" "log" "pg" "toc" "tp" "vr" "vrs"))
   (set (make-local-variable 'tex-start-options) nil)
   (set (make-local-variable 'tex-command) "texi2pdf -b")
   (tex-set-compiler)))

(provide 'mode-texinfo)
