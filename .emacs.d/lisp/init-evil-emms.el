;;; Evil+EMMS
(dolist (mode '(emms-browser-mode emms-playlist-mode))
  (evil-set-initial-state mode 'normal))

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
  (evil-define-key 'normal map
    "+" 'emms-volume-raise
    "=" 'emms-volume-raise
    "-" 'emms-volume-lower
    "u" 'emms-playlist-mode-undo))

(evil-define-key 'normal emms-browser-mode-map
  (kbd "C-<return>") 'emms-browser-add-tracks-and-play
  (kbd "<return>") 'emms-browser-add-tracks
  (kbd "<tab>") 'emms-browser-toggle-subitems
  "/" 'emms-isearch-buffer ; This shows hidden items during search.
  "g1" 'emms-browser-collapse-all
  "g2" 'emms-browser-expand-to-level-2
  "g3" 'emms-browser-expand-to-level-3
  "g4" 'emms-browser-expand-to-level-4
  "<" 'emms-browser-previous-filter
  ">" 'emms-browser-next-filter
  "C" 'emms-browser-clear-playlist
  "D" 'emms-browser-delete-files
  "g0" 'emms-browser-expand-all
  "d" 'emms-browser-view-in-dired
  "\C-j" 'emms-browser-next-non-track
  "\C-k" 'emms-browser-prev-non-track
  "\M-j" 'emms-browser-next-non-track
  "\M-k" 'emms-browser-prev-non-track
  "[" 'emms-browser-prev-non-track
  "]" 'emms-browser-next-non-track
  "{" 'emms-browser-prev-non-track
  "}" 'emms-browser-next-non-track
  "ga" 'emms-browse-by-artist
  "gA" 'emms-browse-by-album
  "gb" 'emms-browse-by-genre
  "gy" 'emms-browse-by-year
  "gc" 'emms-browse-by-composer
  "gp" 'emms-browse-by-performer
  "x" 'emms-pause
  "s" (lookup-key emms-browser-mode-map (kbd "s"))
  "z" (lookup-key emms-browser-mode-map (kbd "W")))

(evil-define-key 'normal emms-playlist-mode-map
  "o" 'evil/emms-playlist-mode-insert-newline-below
  "O" 'evil/emms-playlist-mode-insert-newline-above
  "d" 'emms-playlist-mode-kill-track
  (kbd "<return>") 'emms-playlist-mode-play-smart
  "P" 'evil/emms-playlist-mode-paste-before
  "p" 'evil/emms-playlist-mode-paste-after
  "u" 'emms-playlist-mode-undo
  "<" 'emms-seek-backward
  ">" 'emms-seek-forward
  "C" 'emms-playlist-mode-clear
  "D" 'emms-playlist-mode-kill-track
  "ze" 'emms-tag-editor-edit
  "x" 'emms-pause
  "R" 'emms-tag-editor-rename
  "a" 'emms-playlist-mode-add-contents
  "zp" 'emms-playlist-set-playlist-buffer
  "c" 'emms-playlist-mode-center-current
  "gd" 'emms-playlist-mode-goto-dired-at-point
  "zs" 'emms-show
  "\C-j" 'emms-next
  "\C-k" 'emms-previous
  "\M-j" 'emms-next
  "\M-k" 'emms-previous
  "r" 'emms-random
  "s" 'emms-stop
  "S" (lookup-key emms-playlist-mode-map (kbd "S"))
  "zf" (lookup-key emms-playlist-mode-map (kbd "/"))
  "zff" 'emms-playlist-limit-to-all
  "gg" 'emms-playlist-mode-first
  "G" 'emms-playlist-mode-last
  "]" 'emms-playlist-mode-next
  "[" 'emms-playlist-mode-previous
  "M-y" 'emms-playlist-mode-yank-pop)

(evil-define-key 'visual emms-playlist-mode-map
  "d" 'emms-playlist-mode-kill
  "D" 'emms-playlist-mode-kill)

(provide 'init-evil-emms)
