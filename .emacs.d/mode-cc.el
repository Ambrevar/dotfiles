;;==============================================================================
;; C/C++
;;==============================================================================

(defcustom cc-ldlibs "-lm -pthread"
  "[Local variable] Custom linker flags for C/C++ linkage."
  :safe 'stringp)

(defcustom cc-ldflags ""
  "[Local variable] Custom linker libs for C/C++ linkage."
  :safe 'stringp)

(defun cc-set-compiler ()
  "Set compile command to be nearest Makefile.
The Makefile is looked up in parent folders. If no Makefile is
found, then a configurable command line is provided.\n
Requires `get-closest-pathname'."
  (require 'functions)
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
                   (or (getenv "LDFLAGS") cc-ldflags)
                   (or (getenv "LDLIBS") cc-ldlibs))))))

(defun cc-clean ()
  "Find Makefile and call the `clean' rule. If no Makefile is
found, no action is taken. The previous `compile' command is then
restored."
  (interactive)
  (when (get-closest-pathname)
    (let ((compile-command-backup compile-command))
      (compile (format "make -k -f '%s' clean" (get-closest-pathname)))
      (setq compile-command compile-command-backup))))

;;==============================================================================
;; C-mode
;;==============================================================================

(c-add-style
 "peter"
 '((c-basic-offset . 4)
  (c-comment-only-line-offset . 0)
  (c-hanging-braces-alist
   (brace-list-open)
   (brace-entry-open)
   (substatement-open after)
   (block-close . c-snug-do-while)
   (arglist-cont-nonempty))
  (c-cleanup-list brace-else-brace)
  (c-offsets-alist
   (statement-block-intro . +)
   (knr-argdecl-intro . 0)
   (substatement-open . 0)
   (substatement-label . 0)
   (label . 0)
   (case-label . +)
   (statement-cont . +))))

(mapcar
 (lambda (mode-hook)
   (add-hook
    mode-hook
    (lambda ()
      (c-set-style "peter")
      (cc-set-compiler)
      (guess-style-guess-all)
      (local-set-key (kbd "<f9>") 'cc-clean)
      (local-set-key (kbd "M-TAB") 'semantic-complete-analyze-inline)
      (local-set-key (kbd "C-c C-f") 'cc-fori)
      (local-set-key (kbd "C-c m") 'cc-main)
      (local-set-key (kbd "C-c C-i") 'cc-if)
      (local-set-key (kbd "C-c i") 'cc-include)
      (local-set-key (kbd "C-c I") 'cc-include-local)
      ;; (local-set-key "." 'semantic-complete-self-insert) ; This is a bit slow.
      ;; (local-set-key ">" 'semantic-complete-self-insert)
      (local-set-key (kbd "C-M-e") (lambda () (interactive) (c-beginning-of-defun -1))))))
 '(c-mode-hook c++-mode-hook))

;; WARNING: cc-modes do not need to run hooks for first buffer, since their hook
;; are run twice already. This behaviour may change in future Emacs version, in
;; which case we need to use add-hook-and-eval instead.

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

;; Note that it is possible to extend the skel syntax like this: (setq
;; skeleton-further-elements '((q "\"")))

(define-skeleton cc-printf
  "fprintf/printf snippet.
If no file descriptor is provided, switch do printf.  The format
string is properly parsed (%% are not taken into account).\n
Requires `count-percents'."
  nil
  (require 'functions)
  '(setq v1 (skeleton-read "File desc: " "stderr"))
  (if (string= v1 "") "printf (" (concat "fprintf (" v1 ", "))
  "\"" (setq v1 (skeleton-read "Format string: " "%s\\n")) "\""
  '(setq v2 (count-percents v1))
  '(setq v1 0)
  '(while (< v1 v2)
     (setq v1 (1+ v1))
     (skeleton-insert '(nil (concat ", " (skeleton-read "Value: ")))))
  @ ");")

(define-skeleton cc-fori
  "for i loop."
  nil
  > "for (" @ (skeleton-read "" "i = 0") "; " @ (skeleton-read "" "i < N") "; " @ (skeleton-read "" "i++") ")" \n
  "{" > \n
  @ _ \n
  "}" > )

(define-skeleton cc-main
  "Insert main function with basic includes."
  nil
  > "#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <inttypes.h>

int main(int argc, char** argv)
{" \n
> @ _ \n
> "return 0;
}" \n)

(define-skeleton cc-include
  "Insert system include."
  "Header: "
  \n "#include <" @ str ">" \n)

(define-skeleton cc-include-local
  "Insert local include."
  "Header: "
  \n "#include \"" @ str "\"" \n)

;; TODO: solve indentation issues in if-skeleton.
(define-skeleton cc-if
  "Insert an if statement."
  "Condition: "
  > "if (" @ str ") {" \n
  > @ _ \n
  ( "Other condition, %s: "
    > "} else if (" @ str ") {" > \n
    > @ \n)
  "} else {" > \n
  > @ \n
  resume:
  "}" > \n)

(provide 'mode-cc)
