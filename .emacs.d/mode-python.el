;;==============================================================================
;; Python
;;==============================================================================

(defcustom python-interpreter nil
  "Python interpreter. If value is nil, the shabang will be
checked, and if no shabang is present, `python' will be used."
  :safe 'stringp)

(defun python-version ()
  "Returns whether we're running Python 2 or 3 according to the
shebang. System `python' is assumed by default."
  (let ((firstline
         (car
          (split-string (buffer-substring-no-properties 1 (point-max)) "\n"))))
    (if (not (string-match "^#!" firstline))
        "python"
      (cond
       ((string-match "python2" firstline) "python2")
       ((string-match "python3" firstline) "python3")
       (t "python")))))

(defun python-set-interpreter ()
  "Use compile to run python programs."
  (interactive)
  (hack-local-variables)
  (let ((py-interpreter
         (if python-interpreter
             python-interpreter
           (python-version))))
    (set (make-local-variable 'compile-command)
         (concat py-interpreter " " buffer-file-name))))

(add-hook
 'python-mode-hook
 (lambda ()
   (set (make-local-variable 'compilation-scroll-output) t)
   (python-set-interpreter)))

;; Doc lookup. Requires the python.info file to be installed. See
;; https://bitbucket.org/jonwaltman/pydoc-info/.
;; (add-to-list 'load-path "~/path/to/pydoc-info")
;; (require 'pydoc-info nil t)
