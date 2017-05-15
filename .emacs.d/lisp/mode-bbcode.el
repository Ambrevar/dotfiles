;; BBcode

; For new tags use `bbcode/make-key-binding'.
(local-set-key (kbd "M-RET") 'bbcode/insert-tag-*)

(add-hook-and-eval
 'bbcode-mode-hook
 (lambda ()
   (font-lock-add-keywords nil `((,(bbcode/make-tag-regex "h") . 'font-lock-builtin-face)))
   (font-lock-add-keywords nil `((,(bbcode/make-tag-regex "\\*") . 'font-lock-string-face)))))

(provide 'mode-bbcode)
