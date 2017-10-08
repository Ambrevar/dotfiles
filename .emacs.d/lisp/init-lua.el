;;; Lua

(defvaralias 'lua-indent-level 'tab-width)

(defun lua-set-compiler ()
  (setq compile-command (concat lua-default-application " " (shell-quote-argument buffer-file-name))))

(add-hook 'lua-mode-hook 'lua-set-compiler)

(provide 'init-lua)
