;;; Evil+EMMS

;;; emms-browser does not run any mode hook.  As such the default state is 'normal.
;;; TODO: Report upstream.
(defun evil/emms-browser ()
  (evil-motion-state))
(advice-add 'emms-browser :after 'evil/emms-browser)

(defun evil/emms-playlist-mode-insert-newline-above ()
  "Insert a newline above point."
  (interactive)
  (emms-with-inhibit-read-only-t
   (evil-insert-newline-above)))

(defun evil/emms-playlist-mode-insert-newline-below ()
  "Insert a newline below point."
  (interactive)
  (emms-with-inhibit-read-only-t
   (evil-insert-newline-below)))

(defun evil/emms-playlist-mode-paste-before ()
  "Pastes the latest yanked playlist items before the cursor position.
The return value is the yanked text."
  (interactive)
  (emms-with-inhibit-read-only-t
   (goto-char (point-at-bol))
   (yank)
   (emms-playlist-mode-correct-previous-yank)
   (evil-previous-line)
   (evil-beginning-of-line)))

(defun evil/emms-playlist-mode-paste-after ()
  "Pastes the latest yanked playlist items behind point.
The return value is the yanked text."
  (interactive)
  (evil-next-line)
  (evil/emms-playlist-mode-paste-before))

(dolist (map (list emms-browser-mode-map emms-playlist-mode-map))
  (evil-define-key 'motion map
    "+" 'emms-volume-raise
    "=" 'emms-volume-raise
    "-" 'emms-volume-lower
    "u" 'emms-playlist-mode-undo))

(with-eval-after-load 'emms-browser
  ;; TODO: Why do we need to define emms-browser-mode-map after load and not emms-playlist-mode-map?
  (evil-define-key 'motion emms-browser-mode-map
    (kbd "<return>") 'emms-browser-add-tracks
    (kbd "C-<return>") 'emms-browser-add-tracks-and-play
    "x" 'emms-pause ; TODO: bad binding

    (kbd "<tab>") 'emms-browser-toggle-subitems
    (kbd "SPC") 'emms-browser-toggle-subitems
    "g1" 'emms-browser-collapse-all
    "g2" 'emms-browser-expand-to-level-2
    "g3" 'emms-browser-expand-to-level-3
    "g4" 'emms-browser-expand-to-level-4
    "g0" 'emms-browser-expand-all
    "ga" 'emms-browse-by-artist
    "gA" 'emms-browse-by-album
    "gb" 'emms-browse-by-genre
    "gy" 'emms-browse-by-year
    "gc" 'emms-browse-by-composer
    "gp" 'emms-browse-by-performer

    "/" 'emms-isearch-buffer ; This shows hidden items during search.

    ;; filter
    "<" 'emms-browser-previous-filter
    ">" 'emms-browser-next-filter

    "s" (lookup-key emms-browser-mode-map (kbd "s"))
    "z" (lookup-key emms-browser-mode-map (kbd "W"))

    "C" 'emms-browser-clear-playlist
    "D" 'emms-browser-delete-files
    "d" 'emms-browser-view-in-dired

    ;; motion
    (kbd "C-j") 'emms-browser-next-non-track
    (kbd "C-k") 'emms-browser-prev-non-track
    ;; (kbd "M-j") 'emms-browser-next-non-track ; Custom
    ;; (kbd "M-k") 'emms-browser-prev-non-track ; Custom
    "[" 'emms-browser-prev-non-track
    "]" 'emms-browser-next-non-track))

(evil-set-initial-state 'emms-playlist-mode 'motion)
(evil-define-key 'motion emms-playlist-mode-map
  (kbd "<return>") 'emms-playlist-mode-play-smart
  "x" 'emms-pause ; TODO: bad binding
  "r" 'emms-random
  "s" 'emms-stop
  "]" 'emms-next
  "[" 'emms-previous
  "<" 'emms-seek-backward
  ">" 'emms-seek-forward
  (kbd "C-j") 'emms-next
  (kbd "C-k") 'emms-previous
  ;; (kbd "M-j") 'emms-next ; Custom
  ;; (kbd "M-k") 'emms-previous ; Custom

  ;; motion
  "gg" 'emms-playlist-mode-first
  "G" 'emms-playlist-mode-last
  "]" 'emms-playlist-mode-next
  "[" 'emms-playlist-mode-previous

  ;; "d" 'emms-playlist-mode-kill-track
  "D" 'emms-playlist-mode-kill-track ; emms-browser uses "D"
  "C" 'emms-playlist-mode-clear
  "O" 'evil/emms-playlist-mode-insert-newline-above
  "o" 'evil/emms-playlist-mode-insert-newline-below
  "P" 'evil/emms-playlist-mode-paste-before
  "p" 'evil/emms-playlist-mode-paste-after

  "u" 'emms-playlist-mode-undo

  "ze" 'emms-tag-editor-edit
  "R" 'emms-tag-editor-rename

  "c" 'emms-playlist-mode-center-current
  "gd" 'emms-playlist-mode-goto-dired-at-point ; TODO: Use "d"?

  "zs" 'emms-show
  "a" 'emms-playlist-mode-add-contents
  "zp" 'emms-playlist-set-playlist-buffer

  ;; filter
  "S" (lookup-key emms-playlist-mode-map (kbd "S"))
  "zf" (lookup-key emms-playlist-mode-map (kbd "/"))
  "zff" 'emms-playlist-limit-to-all

  (kbd "M-y") 'emms-playlist-mode-yank-pop)

(evil-define-key 'visual emms-playlist-mode-map
  ;; "d" 'emms-playlist-mode-kill
  "D" 'emms-playlist-mode-kill)

(provide 'init-evil-emms)
