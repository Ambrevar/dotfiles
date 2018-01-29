;;; screencast.el

;; TODO: Add option to remove temp .png.  Toggle with prefix?
;; TODO: Add countdown?
;; TODO: Message which key to use to stop the video.  Make it customizable.  Use
;; minor mode.
;; TODO: Prompt for output path?

(defvar screencast-program "scrot"
  "A program for taking screenshots.")

(defvar screencast-args '("--quality" "25")
  "Arguments to `screencast-program'.
\"scrot\" can use `--focused' to only capture the focused window.")

(defvar screencast-log "*screencast-log*"
  "Name of the buffer logging the actions.
The log is made of the standard output and standard error of the
various programs run here.")

(defvar screencast-convert-program "convert"
  "A program for converting the screenshots to a GIF.")

(defvar screencast-convert-args '("-delay" "100" "-loop" "0" "-dither" "None" "-colors" "80" "-fuzz" "40%" "-layers" "OptimizeFrame")
  "Arguments to `screencast-convert-program'.")

(defvar screencast-optimize-p t
  "If non-nil, run `screencast-optimize-program' over the resulting GIF.")

(defvar screencast-optimize-program "gifsicle"
  "A program for optimizing GIF files.")

(defvar screencast-optimize-args '("--batch" "--optimize=3")
  "Arguments to `screencast-optimize-program'.")

(defvar screencast-output-dir (expand-file-name "Videos/emacs/" "~")
  "Default output directory.")

(defvar screencast--binding-backup nil)
(defvar screencast--frames nil)

;; TODO: Capture on scrolling (e.g. program outputting to Eshell buffer).
;; Use timer?
(defun screencast-capture ()
  (let* ((time (current-time))
         (file (expand-file-name
                (format-time-string "screen-%F-%T-%3N.png" time)
                screencast-output-dir)))
    (apply 'start-process screencast-program (get-buffer-create screencast-log) screencast-program file screencast-args)
    (push (cons time file) screencast--frames)))

(defun screencast-start ()
  (interactive)
  (setq screencast--binding-backup (lookup-key global-map (kbd "<f12>")))
  (global-set-key (kbd "<f12>") 'screencast-stop)
  (unless (file-exists-p screencast-output-dir)
    (make-directory screencast-output-dir))
  (setq screencast--frames '())
  (add-hook 'pre-command-hook 'screencast-capture))

(defun screencast-stop ()
  (interactive)
  (remove-hook 'pre-command-hook 'screencast-capture)
  (global-set-key (kbd "<f12>") screencast--binding-backup)
  (setq screencast--frames (nreverse screencast--frames))
  (let (delays
        (index 0)
        (frames screencast--frames))
    (while (cdr frames)
      (push (list "(" "-clone" (number-to-string index) "-set" "delay"
                  ;; Converters delays expressed in centiseconds.
                  (format "%d" (* 100 (float-time
                                       (time-subtract (car (cadr frames)) (caar frames)))))
                  ")" "-swap" (number-to-string index) "+delete")
            delays)
      (setq index (1+ index)
            frames (cdr frames)))
    (let ((output (expand-file-name
                   (format-time-string "output-%F-%T.gif" (current-time))
                   screencast-output-dir)))
      (apply 'call-process
             screencast-convert-program
             nil (list (get-buffer-create screencast-log) t) nil
             (append
              screencast-convert-args
              (mapcar 'cdr screencast--frames)
              ;; Delays must come after the file arguments.
              (apply 'nconc delays)
              (list output)))
      (when screencast-optimize-p
        (apply 'call-process
               screencast-optimize-program
               nil (list (get-buffer-create screencast-log) t) nil
               (append
                screencast-optimize-args
                (list output)))))))
