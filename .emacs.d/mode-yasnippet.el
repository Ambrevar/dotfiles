;;==============================================================================
;; Yasnippet
;;==============================================================================
;; Yasnippet is slow when loading snippets from source.
;; Generate a bundle instead: yas/compile-bundle
;; Besides you can convert the generated file to bytecode.

;; (add-to-list 'load-path "~/.emacs.d/plugins")
;; (require 'yasnippet-bundle)

;; Next follows a traditional, yet not-optimized configuration for Yasnippet.
;; (add-to-list 'load-path "/usr/share/emacs/site-lisp/yas")
(if (require 'yasnippet nil t)
    (progn
     (setq yas-snippet-dirs "~/.emacs.d/snippets" )

     (if (fboundp 'yas/initialize)
         ;; Old yasnippet versions.
         (progn
           (yas/initialize)
           (yas/load-directory yas-snippet-dirs))

       ;; New yasnippet versions.
       (progn
         (yas-global-mode 1)
         (yas-load-directory yas-snippet-dirs)) ) ))
