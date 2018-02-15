;;; TeX

(dolist (fun '(ambrevar/turn-on-indent-tabs
               ;; ambrevar/turn-on-newline-paragraph
               prettify-symbols-mode))
  (add-hook 'tex-mode-hook fun))

(defun ambrevar/tex-toggle-escape-char ()
  "Make backslash part of the word syntax or not.
This does not interfere with `subword-mode'."
  (interactive)
  (if (equal (char-syntax ?\\) ?\\)
      (progn
        (modify-syntax-entry ?\\ "w")
        (message "\\ is a not an escape character"))
    (modify-syntax-entry ?\\ "\\")
    (message "\\ is a an escape character")))

(with-eval-after-load 'tex ; AUCTeX
  (setq TeX-auto-save t
        TeX-parse-self t
        ;; Use pdf-tools to open PDF files
        TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-source-correlate-start-server t
        ;; TeX-fold-auto t
        TeX-electric-escape t
        ;; TeX-electric-math t
        TeX-electric-sub-and-superscript t)
  (setq-default TeX-master nil)

  (when (require 'helm-config nil t)
    (define-key TeX-mode-map (kbd "M-s f") 'helm-semantic-or-imenu))

  ;; Update PDF buffers after successful LaTeX runs
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer))

;;; Not sure how useful that is:
;; (set (make-local-variable 'use-hard-newlines) t)

(provide 'init-tex)
