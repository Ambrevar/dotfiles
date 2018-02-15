;;; Dired
;;; WARNING: This file is loaded unconditionally on startup.
;;; We cannot assume that current buffer is in dired-mode.

(ambrevar/define-keys dired-mode-map
                      "C-c h" 'ambrevar/dired-toggle-humansize
                      "<left>" 'dired-up-directory
                      "<right>" 'dired-find-file
                      "SPC" 'dired-mark
                      "<backspace>" 'dired-up-directory
                      "b" 'dired-up-directory)

(when (require 'dired+ nil t)
  (toggle-diredp-find-file-reuse-dir 1))

;;; On a GNU system, ls has the option to sort folders first.
(if (string-match "^gnu.*" (prin1-to-string system-type))
    (setq dired-listing-switches "--group-directories-first -lha")
  (setq dired-listing-switches "-lha"))

;;; Switches are set before the hook is called, so we need to reload dired. The
;;; dired-internal-noselect is a lower level function, so it is faster. WARNING:
;;; Not sure if it is equivalent though.
;; (dired dired-directory dired-listing-switches)
(defun ambrevar/dired-set-listing-switches ()
  (dired-internal-noselect dired-directory dired-listing-switches))

(setq wdired-allow-to-change-permissions t)

;;; omit-mode needs to be started _after_ omit-files redefinition.
(require 'dired-x)
(setq dired-omit-files "^\\.")

(setq dired-guess-shell-alist-user
      (list
       '("\\.ogg$" "mpv")
       '("\\.\\(jpe?g\\|png\\|git\\)$" "sxiv")
       '("\\.\\(mkv\\|mpe?g\\|avi\\|mp4\\|ogm\\)$" "mpv")))

(defvar ambrevar/dired-showing-humansize t "If dired is displaying humansize or not.")

(defun ambrevar/dired-toggle-humansize ()
  "Toggle displaying humansize in dired."
  (interactive)
  (let ((switch-regexp "\\(\\`\\| \\)-\\([a-gi-zA-Z]*\\)\\(h\\)\\([^ ]*\\)")
        case-fold-search)
    (while (string-match switch-regexp dired-actual-switches)
      (if (and (equal (match-string 2 dired-actual-switches) "")
               (equal (match-string 4 dired-actual-switches) ""))
          (setq dired-actual-switches
                (replace-match "" t t dired-actual-switches))
        (setq dired-actual-switches
              (replace-match "" t t dired-actual-switches 3))))
    (if ambrevar/dired-showing-humansize
        (setq ambrevar/dired-showing-humansize nil)
      (progn
        (setq dired-actual-switches
              (concat dired-actual-switches
                      (if (string-match-p "\\`-[[:alnum:]]+\\'"
                                          dired-actual-switches)
                          "h" " -h")))
        (setq ambrevar/dired-showing-humansize t))))
  (revert-buffer))

(dolist (fun '(dired-omit-mode ambrevar/dired-set-listing-switches))
  (add-hook 'dired-mode-hook fun))

(provide 'init-dired)
