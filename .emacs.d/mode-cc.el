;;==============================================================================
;; C/C++
;;==============================================================================
(require 'compile)

;; We need some functions:
;; (get-closest-pathname)
;; (count-percents)
(require 'functions)

(defcustom mode-cc-ldlibs "-lm -pthread"
  "[Local variable] Custom linker flags for C/C++ linkage."
  :safe 'stringp)

(defcustom mode-cc-ldflags ""
  "[Local variable] Custom linker libs for C/C++ linkage."
  :safe 'stringp)

(defun cc-set-compiler ()
  "Set C/C++ compile command to be nearest Makefile found in
parent folders. If no Makefile is found, then a configurable
command line is provided."
  (interactive)
  (if (get-closest-pathname)
      (set (make-local-variable 'compile-command) (format "make -k -f %s" (get-closest-pathname)))
    (set (make-local-variable 'compile-command)
         (let
             ((is-cpp (equal (symbol-name major-mode) "c++-mode"))
              (file (file-name-nondirectory buffer-file-name)))
           (format "%s %s -o %s %s %s %s"
                   (if is-cpp
                       (or (getenv "CXX") "g++")
                     (or (getenv "CC") "gcc"))
                   file
                   (file-name-sans-extension file)
                   (if is-cpp
                       (or (getenv "CPPFLAGS") "-Wall -Wextra -Wshadow -DDEBUG=9 -g3 -O0")
                     (or (getenv "CFLAGS") "-ansi -pedantic -std=c99 -Wall -Wextra -Wshadow -DDEBUG=9 -g3 -O0"))
                   (or (getenv "LDFLAGS") mode-cc-ldflags)
                   (or (getenv "LDLIBS") mode-cc-ldlibs))))))

(defun cc-clean ()
  "Find Makefile and call the `clean' rule. If no Makefile is
found, no action is taken. The previous `compile' command is then
restored."
  (interactive)
  (when (get-closest-pathname)
    (let ((compile-command-backup compile-command))
      (compile (format "make -k -f %s clean" (get-closest-pathname)))
      (setq compile-command compile-command-backup))))

;;==============================================================================
;; C-mode
;;==============================================================================

(mapcar
 (lambda (mode-hook)
   (add-hook
    mode-hook
    (lambda ()
      ;; Identation style
      (c-set-style "linux")
      (setq c-basic-offset 4)

      (cc-set-compiler)
      (local-set-key (kbd "<f9>") 'cc-clean)
      (local-set-key (kbd "M-TAB") 'semantic-complete-analyze-inline)
      (local-set-key (kbd "C-c C-f") 'snip-cc-fori)
      ;; (local-set-key "." 'semantic-complete-self-insert) ; This is a bit slow.
      ;; (local-set-key ">" 'semantic-complete-self-insert)
      (local-set-key (kbd "C-M-e") (lambda () (interactive) (c-beginning-of-defun -1))))))
 '(c-mode-hook c++-mode-hook))

;; TODO: test this!
;; (defun vlad-cc-style()
;;   (c-set-style "linux")
;;   (c-set-offset 'innamespace '0)
;;   (c-set-offset 'inextern-lang '0)
;;   (c-set-offset 'inline-open '0)
;;   (c-set-offset 'label '*)
;;   (c-set-offset 'case-label '*)
;;   (c-set-offset 'access-label '/)
;;   (setq c-basic-offset 4)
;;   (setq tab-width 4)
;;   (setq indent-tabs-mode nil)
;; )
;; (add-hook 'c++-mode-hook 'vlad-cc-style)

;;==============================================================================
;; Qt semantic support
;;==============================================================================

;; Qt base directory, meaning the directory where the 'Qt' directory can be found.
;; Adapt accordingly.
(when (fboundp 'semantic-add-system-include)
  (setq qt4-base-dir "/usr/include/qt4")
  (setq qt4-gui-dir (concat qt4-base-dir "/QtGui"))
  (semantic-add-system-include qt4-base-dir 'c++-mode)
  (semantic-add-system-include qt4-gui-dir 'c++-mode)
  (add-to-list 'auto-mode-alist (cons qt4-base-dir 'c++-mode))
  (add-hook
   'c++-mode-hook
   (lambda ()
     (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qconfig.h"))
     (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qconfig-large.h"))
     (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qglobal.h")))))

;;==============================================================================
;; Skel
;;==============================================================================
;; TODO: elements: (setq skeleton-further-elements '((q "\"")))
(define-skeleton snip-cc-printf
  "fprintf/printf snippet

If no file descriptor is provided, switch do printf.  The format
string is properly parsed (%% are not taken into account).

Requires the `count-percents' function."
  nil
  '(setq v1 (skeleton-read "File desc: " "stderr"))
  (if (string= v1 "") "printf (" (concat "fprintf (" v1 ", "))
  "\"" (setq v1 (skeleton-read "Format string: " "%s\\n")) "\""
  '(setq v2 (count-percents v1))
  '(setq v1 0)
  '(while (< v1 v2)
     (setq v1 (1+ v1))
     (skeleton-insert '(nil (concat ", " (skeleton-read "Value: ")))))
  @ ");")

(define-skeleton snip-cc-fori
  "for i loop."
  nil
  > "for (" @ (skeleton-read "" "i = 0") "; " @ (skeleton-read "" "i < N") "; " @ (skeleton-read "" "i++") ")" \n
  "{" > \n
  @ _ \n
  "}" > )

(provide 'mode-cc)
