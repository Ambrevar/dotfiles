;;==============================================================================
;; Python
;;==============================================================================

(defcustom python-compiler ""
  "Python compiler."
  :safe 'stringp)

(defun python-version ()
  "Returns whether we're running Python 2 or 3 according to the
shebang. System `python' is assumed by default."
  (let ((firstline (car (split-string (buffer-string) "\n" t))))
    (if (not (string-match "^#!" firstline))
        "python"
      (cond
       ((string-match "python2" firstline) "python2")
       ((string-match "python3" firstline) "python3")
       (t "python")))))

(defun python-compile ()
  "Use compile to run python programs."
  (interactive)
  (let ((py-compiler
         (if (equal python-compiler "")
             (python-version)
           python-compiler)))
    (save-buffer)
    (compile (concat py-compiler " '" buffer-file-name "'"))))

(add-hook
 'python-mode-hook
 (lambda ()
   (set (make-local-variable 'compilation-scroll-output) t)
   (local-set-key (kbd "<f10>") 'python-compile)) t)

;; Doc lookup. Requires the python.info file to be installed. See
;; https://bitbucket.org/jonwaltman/pydoc-info/.
(add-to-list 'load-path "~/path/to/pydoc-info")
(require 'pydoc-info nil t)
