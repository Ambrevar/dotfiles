;;==============================================================================
;; C/C++
;;==============================================================================
(require 'compile)

(defun get-closest-pathname (&optional file)
  "Determine the pathname of the first instance of FILE starting
from the current directory towards root.  This may not do the
correct thing in presence of links. If it does not find FILE,
then it shall return the name of FILE in the current directory,
suitable for creation"
  (let ((current-dir default-directory) (looping t) (makefile (or file "Makefile")))
    (while (progn
             (if (file-exists-p (expand-file-name makefile current-dir))
                 (setq looping nil)
               (setq current-dir (expand-file-name ".." current-dir)))
             (and looping (not (equal current-dir "/")))))
    (if (equal current-dir "/") nil (expand-file-name makefile current-dir))))


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

;; Identation style
(setq c-default-style "linux" c-basic-offset 4)

(add-hook
 'c-mode-hook
 (lambda ()
   (cc-set-compiler)
   (local-set-key (kbd "<f9>") 'cc-clean)
   (local-set-key (kbd "M-TAB") 'semantic-complete-analyze-inline)
   (local-set-key (kbd "C-M-e") (lambda () (interactive) (c-beginning-of-defun -1)))
   ;; (local-set-key "." 'semantic-complete-self-insert) ; This is a bit slow.
   ;; (local-set-key ">" 'semantic-complete-self-insert)
   ))

;;==============================================================================
;; C++-mode
;;==============================================================================
(require 'compile)

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

(add-hook
 'c++-mode-hook
 (lambda ()
   (cc-set-compiler)
   (local-set-key (kbd "<f9>") 'cc-clean)
   (local-set-key (kbd "C-M-e") (lambda () (interactive) (c-beginning-of-defun -1)))
   (local-set-key (kbd "M-TAB") 'semantic-complete-analyze-inline)))

;; autoinsert C/C++ header
(define-auto-insert
  (cons "\\.\\([Hh]\\|hh\\|hpp\\)\\'" "My C / C++ header")
  '(nil
    "/" (make-string 79 ?*) "\n"
    " * @file " (file-name-nondirectory buffer-file-name) "\n"
    " * @date \n"
    " * @brief \n"
    " *\n"
    " " (make-string 78 ?*) "/\n\n"
    (let* ((noext (substring buffer-file-name 0 (match-beginning 0)))
           (nopath (file-name-nondirectory noext))
           (ident (concat (upcase nopath) "_H")))
      (concat "#ifndef " ident "\n"
              "#define " ident  " 1\n\n\n"
              "\n\n#endif // " ident "\n"))))

;; auto insert C/C++
(define-auto-insert
  (cons "\\.\\([Cc]\\|cc\\|cpp\\)\\'" "My C++ implementation")
  '(nil
    "/" (make-string 79 ?*) "\n"
    " * @file " (file-name-nondirectory buffer-file-name) "\n"
    " * @date \n"
    " * @brief \n"
    " *\n"
    " " (make-string 78 ?*) "/\n\n"
    (let* ((noext (substring buffer-file-name 0 (match-beginning 0)))
           (nopath (file-name-nondirectory noext))
           (ident (concat nopath ".h")))
      (if (file-exists-p ident)
          (concat "#include \"" ident "\"\n")))))

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
