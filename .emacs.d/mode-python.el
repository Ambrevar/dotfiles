;;==============================================================================
;; Python
;;==============================================================================

(defcustom python-compiler "python"
  "Python compiler."
  :safe 'stringp)

(add-hook
 'python-mode-hook
 (lambda ()
   (defun python-compile ()
     "Use compile to run python programs."
     (interactive)
     (compile (concat python-compiler " \"" buffer-file-name "\""))
     )
   (setq compilation-scroll-output t)
   (local-set-key "\C-c\C-c" 'python-compile)
   ))

;; Doc lookup. Requires the python.info file to be installed. See
;; https://bitbucket.org/jonwaltman/pydoc-info/.
(add-to-list 'load-path "~/path/to/pydoc-info")
(require 'pydoc-info nil t)
