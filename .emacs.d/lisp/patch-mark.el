;;; Fix https://github.com/emacs-helm/helm/issues/1891
;;; TODO: Report upstream?
(defun tv/advice-push-mark (&optional location nomsg activate)
  (unless (null (mark t))
    (let ((marker (copy-marker (mark-marker))))
      (setq mark-ring (cons marker (delete marker mark-ring))))
    (when (> (length mark-ring) mark-ring-max)
      ;; Move marker to nowhere.
      (set-marker (car (nthcdr mark-ring-max mark-ring)) nil)
      (setcdr (nthcdr (1- mark-ring-max) mark-ring) nil)))
  (set-marker (mark-marker) (or location (point)) (current-buffer))
  ;; Now push the mark on the global mark ring.
  (setq global-mark-ring (cons (copy-marker (mark-marker))
                               ;; Avoid having multiple entries
                               ;; for same buffer in `global-mark-ring'.
                               (cl-loop with mb = (current-buffer)
                                        for m in global-mark-ring
                                        for nmb = (marker-buffer m)
                                        unless (eq mb nmb)
                                        collect m)))
  (when (> (length global-mark-ring) global-mark-ring-max)
    (set-marker (car (nthcdr global-mark-ring-max global-mark-ring)) nil)
    (setcdr (nthcdr (1- global-mark-ring-max) global-mark-ring) nil))
  (or nomsg executing-kbd-macro (> (minibuffer-depth) 0)
      (message "Mark set"))
  (when (or activate (not transient-mark-mode))
    (set-mark (mark t)))
  nil)
(advice-add 'push-mark :override #'tv/advice-push-mark)

(provide 'patch-mark)
