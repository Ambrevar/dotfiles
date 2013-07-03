;; This configuration is in a seperate file so that we can choose to load it or
;;not.

;;==============================================================================
;;Yasnippet
;;==============================================================================
;;Yasnippet is slow when loading snippets from source.  Generate a bundle
;;instead: yas/compile-bundle Besides you can convert the generated file to
;;bytecode.

;; (add-to-list 'load-path "~/.emacs.d/plugins")
;; (require 'yasnippet-bundle)

;; Next follows a traditional, yet not-optimized configuration for Yasnippet.
;; (add-to-list 'load-path "/usr/share/emacs/site-lisp/yas")
(when (require 'yasnippet nil t)
  (setq yas-snippet-dirs "~/.emacs.d/snippets" )
  (if (fboundp 'yas/initialize)
      ;; Old yasnippet versions.
      (progn
        (yas/initialize)
        (set-face-background 'yas/field-highlight-face "#1a1a1a")
        (yas/load-directory yas-snippet-dirs))

    ;; New yasnippet versions.
    (progn
      (yas-global-mode 1)
      (yas-load-directory yas-snippet-dirs)) ) )
