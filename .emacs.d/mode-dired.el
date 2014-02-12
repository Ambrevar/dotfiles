;; Dired options
;; On a GNU system, ls has the option to sort folders first.
(if (string-match "^gnu.*" (prin1-to-string system-type))
    (setq dired-listing-switches "--group-directories-first -lh")
  (setq dired-listing-switches "-lh"))
(setq wdired-allow-to-change-permissions t)

(defvar dired-showing-hidden nil "If dired is displaying hidden files or not.")
(defvar dired-showing-humansize t "If dired is displaying humansize or not.")

(defun dired-toggle-hidden ()
  "Toggle displaying hidden files in dired."
  (interactive)
  (let (;; Regexp for finding (possibly embedded) -a switches.
        (switch-regexp "\\(\\`\\| \\)-\\([b-zA-Z]*\\)\\(a\\)\\([^ ]*\\)")
        case-fold-search)
    ;; Remove the -a switch.
    (while (string-match switch-regexp dired-actual-switches)
      (if (and (equal (match-string 2 dired-actual-switches) "")
               (equal (match-string 4 dired-actual-switches) ""))
          ;; Remove a stand-alone -a switch.
          (setq dired-actual-switches
                (replace-match "" t t dired-actual-switches))
        ;; Remove a switch of the form -XaY for some X and Y.
        (setq dired-actual-switches
              (replace-match "" t t dired-actual-switches 3))))
    ;; Now, if we weren't sorting by date before, add the -a switch.  Some
    ;; simple-minded ls implementations (eg ftp servers) only allow a single
    ;; option string, so try not to add " -a" if possible.
    (if dired-showing-hidden
        (setq dired-showing-hidden nil)
      (progn
        (setq dired-actual-switches
              (concat dired-actual-switches
                      (if (string-match-p "\\`-[[:alnum:]]+\\'"
                                          dired-actual-switches)
                          "a" " -a")))
        (setq dired-showing-hidden t))))
  ;; (dired-sort-set-mode-line)
  (revert-buffer))

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

(local-set-key (kbd "C-c a") 'dired-toggle-hidden)
(local-set-key (kbd "C-c h") 'dired-toggle-humansize)
(local-set-key (kbd "<left>") 'dired-up-directory)
(local-set-key (kbd "<right>") 'dired-find-file)
(local-set-key (kbd "SPC") 'dired-mark)
(local-set-key (kbd "<backspace>") 'dired-up-directory)
(local-set-key (kbd "b") 'dired-up-directory)))

(provide 'mode-dired)
