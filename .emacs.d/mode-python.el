;;==============================================================================
;; Python
;;==============================================================================

(defcustom python-compiler ""
  "Python compiler."
  :safe 'stringp)

(defun is-python3-p () "Check whether we're running Python 2 or
3. Python is assumed by default."
  (let ((firstline (car (split-string (buffer-string) "\n" t))))
    (let ((firstword (car (split-string firstline nil t))))
      (let ((version  (if (string-match "/bin/env" firstword)
                          (car (cdr (split-string firstline nil t)))
                        firstword)))
        (if (string-match "python2" version) nil t)))))

(defun python-compile ()
  "Use compile to run python programs."
  (interactive)
  (let ((py-compiler
         (if (equal python-compiler "")
             (if (is-python3-p)
                 "python3" "python2")
           python-compiler)))
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
