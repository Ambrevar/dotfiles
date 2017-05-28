;; Dired

(local-set-key (kbd "C-c h") 'dired-toggle-humansize)
(local-set-key (kbd "<left>") 'dired-up-directory)
(local-set-key (kbd "<right>") 'dired-find-file)
(local-set-key (kbd "SPC") 'dired-mark)
(local-set-key (kbd "<backspace>") 'dired-up-directory)
(local-set-key (kbd "b") 'dired-up-directory)

(when (require 'dired+ nil t)
  (toggle-diredp-find-file-reuse-dir 1))

;; On a GNU system, ls has the option to sort folders first.
(if (string-match "^gnu.*" (prin1-to-string system-type))
    (setq dired-listing-switches "--group-directories-first -lha")
  (setq dired-listing-switches "-lha"))

;; Switches are set before the hook is called, so we need to reload dired. The
;; dired-internal-noselect is a lower level function, so it is faster. WARNING:
;; Not sure if it is equivalent though.
; (dired dired-directory dired-listing-switches)
(dired-internal-noselect dired-directory dired-listing-switches)

(setq wdired-allow-to-change-permissions t)

;; omit-mode needs to be started _after_ omit-files redefinition.
(require 'dired-x)
(setq dired-omit-files "^\\.")
(dired-omit-mode)
(add-hook 'dired-mode-hook 'dired-omit-mode)

(require 'tool-pdf) ; for `pdf-viewer'
(setq dired-guess-shell-alist-user
      (list
       '("\\.ogg$" "mpv")
       '("\\.\\(jpe?g\\|png\\|git\\)$" "sxiv")
       '("\\.\\(mkv\\|mpe?g\\|avi\\|mp4\\|ogm\\)$" "mpv")
       '("\\.pdf$" (concat pdf-viewer " " (mapconcat 'identity pdf-viewer-args " ")))))

(defvar dired-showing-humansize t "If dired is displaying humansize or not.")

(defun dired-toggle-humansize ()
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
    (if dired-showing-humansize
        (setq dired-showing-humansize nil)
      (progn
        (setq dired-actual-switches
              (concat dired-actual-switches
                      (if (string-match-p "\\`-[[:alnum:]]+\\'"
                                          dired-actual-switches)
                          "h" " -h")))
        (setq dired-showing-humansize t))))
  (revert-buffer))

(provide 'mode-dired)
