;; Read Matlab files in Octave mode.
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;; Set comments to be '%'. TODO: does not work?
(setq octave-comment-char 37)
(setq octave-comment-start "% ")

(provide 'mode-octave)
