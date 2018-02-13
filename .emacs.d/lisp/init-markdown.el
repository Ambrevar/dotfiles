;;; Markdown

(set-face-attribute 'markdown-header-face-1 nil :inherit 'info-title-1)
(set-face-attribute 'markdown-header-face-2 nil :inherit 'info-title-2)
(set-face-attribute 'markdown-header-face-3 nil :inherit 'info-title-3)
(set-face-attribute 'markdown-header-face-4 nil :inherit 'info-title-4)

(define-key markdown-mode-map "\M-'" 'markdown-blockquote-region)

(add-hook 'markdown-mode-hook 'ambrevar/turn-on-newline-paragraph)

(provide 'init-markdown)
