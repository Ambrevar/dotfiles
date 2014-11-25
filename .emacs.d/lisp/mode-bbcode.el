;;==============================================================================
;; BBcode
;;==============================================================================

(add-hook-and-eval
 'bbcode-mode-hook
 (lambda ()
   (local-set-key (kbd "C-c C-b") 'bbcode/insert-tag-b)
   (local-set-key (kbd "C-c C-u") 'bbcode/insert-tag-url)
   (local-set-key (kbd "C-c C-i") 'bbcode/insert-tag-i)
   (font-lock-add-keywords nil `((,(bbcode/make-tag-regex "h") . 'font-lock-builtin-face)))
   (font-lock-add-keywords nil `((,(bbcode/make-tag-regex "\\*") . 'font-lock-string-face)))))

(bbcode/make-key-binding "M-RET" "*")
(bbcode/make-key-binding "C-c C-h" "h")

(provide 'mode-bbcode)
