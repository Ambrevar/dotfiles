;; Lua

(defvaralias 'lua-indent-level 'tab-width)

(add-hook-and-eval
 'lua-mode-hook
 (lambda ()
   (set (make-local-variable 'compile-command) (concat lua-default-application " " (shell-quote-argument buffer-file-name)))))

(provide 'mode-lua)
