;;==============================================================================
;; Graphviz' dot mode
;;==============================================================================

(autoload 'graphviz-dot-mode "graphviz-dot-mode.el" "Graphviz dot mode." t)
(add-to-list 'auto-mode-alist '("\\.dot\\'" . graphviz-dot-mode))


(setq graphviz-dot-preview-extension "pdf")
(defcustom graphviz-dot-view-extension "pdf"
  "Graphviz format for external view."
  :safe 'stringp)
(setq graphviz-dot-view-command "zathura --fork %s")

;; Override original stupid function.
(add-hook
 'graphviz-dot-mode-hook
 (lambda ()
   (defun graphviz-dot-view ()
     "Runs an external viewer. This creates an external process every time it
is executed. If `graphviz-dot-save-before-view' is set, the current
buffer is saved before the command is executed."
     (interactive)
     (let ((cmd (if graphviz-dot-view-edit-command
                    (read-from-minibuffer
                     "View command: "
                     (format graphviz-dot-view-command
                             (concat (file-name-sans-extension buffer-file-name)
                                     "." graphviz-dot-view-extension)))
                  (format graphviz-dot-view-command
                          (concat (file-name-sans-extension buffer-file-name)
                                  "." graphviz-dot-view-extension)))))
       (if graphviz-dot-save-before-view
           (save-buffer))
       (setq novaproc (start-process-shell-command
                       (downcase mode-name) nil cmd))
       (message (format "Executing `%s'..." cmd))))))


