;;; Fix bug#29157: 25.3; Eshell parsing fails sometimes [...]
(with-eval-after-load 'em-hist
  (defun eshell-hist-initialize ()
    "Initialize the history management code for one Eshell buffer."
    ;; PATCH: Disable the history references.
    ;; (add-hook 'eshell-expand-input-functions
    ;; 'eshell-expand-history-references nil t)

    (when (eshell-using-module 'eshell-cmpl)
      (add-hook 'pcomplete-try-first-hook
                'eshell-complete-history-reference nil t))

    (if (and (eshell-using-module 'eshell-rebind)
             (not eshell-non-interactive-p))
        (let ((rebind-alist eshell-rebind-keys-alist))
          (make-local-variable 'eshell-rebind-keys-alist)
          (setq eshell-rebind-keys-alist
                (append rebind-alist eshell-hist-rebind-keys-alist))
          (set (make-local-variable 'search-invisible) t)
          (set (make-local-variable 'search-exit-option) t)
          (add-hook 'isearch-mode-hook
                    (function
                     (lambda ()
                       (if (>= (point) eshell-last-output-end)
                           (setq overriding-terminal-local-map
                                 eshell-isearch-map)))) nil t)
          (add-hook 'isearch-mode-end-hook
                    (function
                     (lambda ()
                       (setq overriding-terminal-local-map nil))) nil t))
      (define-key eshell-mode-map [up] 'eshell-previous-matching-input-from-input)
      (define-key eshell-mode-map [down] 'eshell-next-matching-input-from-input)
      (define-key eshell-mode-map [(control up)] 'eshell-previous-input)
      (define-key eshell-mode-map [(control down)] 'eshell-next-input)
      (define-key eshell-mode-map [(meta ?r)] 'eshell-previous-matching-input)
      (define-key eshell-mode-map [(meta ?s)] 'eshell-next-matching-input)
      (define-key eshell-command-map [(meta ?r)]
        'eshell-previous-matching-input-from-input)
      (define-key eshell-command-map [(meta ?s)]
        'eshell-next-matching-input-from-input)
      (if eshell-hist-match-partial
          (progn
            (define-key eshell-mode-map [(meta ?p)]
              'eshell-previous-matching-input-from-input)
            (define-key eshell-mode-map [(meta ?n)]
              'eshell-next-matching-input-from-input)
            (define-key eshell-command-map [(meta ?p)] 'eshell-previous-input)
            (define-key eshell-command-map [(meta ?n)] 'eshell-next-input))
        (define-key eshell-mode-map [(meta ?p)] 'eshell-previous-input)
        (define-key eshell-mode-map [(meta ?n)] 'eshell-next-input)
        (define-key eshell-command-map [(meta ?p)]
          'eshell-previous-matching-input-from-input)
        (define-key eshell-command-map [(meta ?n)]
          'eshell-next-matching-input-from-input)))

    (make-local-variable 'eshell-history-size)
    (or eshell-history-size
        (let ((hsize (getenv "HISTSIZE")))
          (setq eshell-history-size
                (if (and (stringp hsize)
                         (integerp (setq hsize (string-to-number hsize)))
                         (> hsize 0))
                    hsize
                  128))))

    (make-local-variable 'eshell-history-file-name)
    (or eshell-history-file-name
        (setq eshell-history-file-name (getenv "HISTFILE")))

    (make-local-variable 'eshell-history-index)
    (make-local-variable 'eshell-save-history-index)

    (if (minibuffer-window-active-p (selected-window))
        (set (make-local-variable 'eshell-save-history-on-exit) nil)
      (set (make-local-variable 'eshell-history-ring) nil)
      (if eshell-history-file-name
          (eshell-read-history nil t))

      (add-hook 'eshell-exit-hook 'eshell-write-history nil t))

    (unless eshell-history-ring
      (setq eshell-history-ring (make-ring eshell-history-size)))

    (add-hook 'eshell-exit-hook 'eshell-write-history nil t)

    (add-hook 'kill-emacs-hook 'eshell-save-some-history)

    (make-local-variable 'eshell-input-filter-functions)
    (add-hook 'eshell-input-filter-functions 'eshell-add-to-history nil t)

    (define-key eshell-command-map [(control ?l)] 'eshell-list-history)
    (define-key eshell-command-map [(control ?x)] 'eshell-get-next-from-history)))

;;; Fix 29854, expected in Emacs 26.1?
(setq
 ansi-color-apply-face-function
 (lambda (beg end face)
   (when face
     (put-text-property beg end 'face face))))

;;; REVIEW: Ignore dups in the entire ring, not just the last entry.
;;; Reported upstream, see #30466.
(defun eshell-add-input-to-history (input)
  "Add the string INPUT to the history ring.
Input is entered into the input history ring, if the value of
variable `eshell-input-filter' returns non-nil when called on the
input."
  (when (funcall eshell-input-filter input)
    (when eshell-hist-ignoredups
      (ring-remove eshell-history-ring
                   (ring-member eshell-history-ring input)))
    (eshell-put-history input))
  (setq eshell-save-history-index eshell-history-index)
  (setq eshell-history-index nil))

(provide 'patch-eshell-26)
