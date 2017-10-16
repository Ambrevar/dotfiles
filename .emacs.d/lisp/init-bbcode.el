;;; BBcode

;;; For new tags use `bbcode/make-key-binding'.
(define-key bbcode-mode-map (kbd "M-<return>") 'bbcode/insert-tag-*)

(font-lock-add-keywords nil `((,(bbcode/make-tag-regex "h") . 'font-lock-builtin-face)))
(font-lock-add-keywords nil `((,(bbcode/make-tag-regex "\\*") . 'font-lock-string-face)))

(provide 'init-bbcode)
