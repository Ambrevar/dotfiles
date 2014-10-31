;; Temp fix:
;; http://blog.gmane.org/gmane.emacs.cc-mode.general
;; (message "@@@ LOADED")
;; (defmacro c-with-all-but-one-cpps-commented-out (beg end &rest forms)
;;   ;; Execute FORMS... whilst the syntactic effect of all characters in
;;   ;; every CPP region APART FROM THE ONE BETWEEN BEG and END is
;;   ;; suppressed.
;;   (message "@@@ C FIX")
;;   `(unwind-protect
;;        (c-save-buffer-state ()
;;          (save-restriction
;;            (widen)
;;            (c-clear-cpp-delimiters ,beg ,end))
;;          ,`(c-with-cpps-commented-out ,@forms))
;;      (c-save-buffer-state ()
;;        (save-restriction
;;          (widen)
;;          (c-set-cpp-delimiters ,beg ,end)))))

;; Org Mode
(eval-after-load "org"
  '(progn
     (add-to-list
      'org-agenda-files "~/projects/kth-ta/agenda.org")))
