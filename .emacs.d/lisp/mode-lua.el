;;==============================================================================
;; Lua
;;==============================================================================

(defvaralias 'lua-indent-level 'tab-width)

(add-hook-and-eval
 'lua-mode-hook
 (lambda ()
   (set (make-local-variable 'compile-command) (format "lua '%s'" buffer-file-name))))

(provide 'mode-lua)
