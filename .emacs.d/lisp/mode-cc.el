;;==============================================================================
;; C/C++
;;==============================================================================
;; Note: maybe this should be split in mode-c and mode-c++.

(defcustom cc-ldlibs "-lm -pthread"
  "Custom linker flags for C/C++ linkage."
  :safe 'stringp)
(make-variable-buffer-local 'cc-ldlibs)

(defcustom cc-ldflags ""
  "Custom linker libs for C/C++ linkage."
  :safe 'stringp)
(make-variable-buffer-local 'cc-ldflags)

(defun cc-set-compiler (&optional nomakefile)
  "Set compile command to be nearest Makefile or a generic command.
The Makefile is looked up in parent folders. If no Makefile is
found (or if NOMAKEFILE is non-nil or if function was called with
universal argument), then a configurable command line is
provided.\n Requires `get-closest-pathname'."
  (interactive "P")
  (require 'functions)
  (hack-local-variables)
  (let ((makefile (get-closest-pathname)))
    (if (and makefile
            (not nomakefile))
        (set (make-local-variable 'compile-command) (format "make -k -C %s" (file-name-directory makefile)))
      (set (make-local-variable 'compile-command)
           (let
               ((c++-p (eq major-mode 'c++-mode))
                (file (file-name-nondirectory buffer-file-name)))
             (format "%s %s -o %s %s %s %s"
                     (if c++-p
                         (or (getenv "CXX") "g++")
                       (or (getenv "CC") "gcc"))
                     file
                     (file-name-sans-extension file)
                     (if c++-p
                         (or (getenv "CPPFLAGS") "-Wall -Wextra -Wshadow -DDEBUG=9 -g3 -O0")
                       (or (getenv "CFLAGS") "-ansi -pedantic -std=c99 -Wall -Wextra -Wshadow -DDEBUG=9 -g3 -O0"))
                     (or (getenv "LDFLAGS") cc-ldflags)
                     (or (getenv "LDLIBS") cc-ldlibs)))))))

(defun cc-clean ()
  "Find Makefile and call the `clean' rule. If no Makefile is
found, no action is taken. The previous `compile' command is then
restored."
  (interactive)
  (when (get-closest-pathname)
    (let ((compile-command-backup compile-command))
      (compile (format "make -k -f '%s' clean" (get-closest-pathname)))
      (setq compile-command compile-command-backup))))

(defun cc-fmt ()
  "Run uncrustify(1) on current buffer."
  (interactive)
  (let ((status)
        (formatbuf (get-buffer-create "*C format buffer*")))
    (setq status
          (call-process-region (point-min) (point-max) "uncrustify" nil formatbuf nil "-lc" "-q"))
    (if (/= status 0)
        (error "Bad formatted C file")
      (delete-region (point-min) (point-max))
      (insert-buffer formatbuf)
      (kill-buffer formatbuf))))

;;==============================================================================
;; C-mode
;;==============================================================================

;; WARNING: this style is a work-in-progress.
(c-add-style
 "peter"
 `((c-comment-only-line-offset . 0)
   (c-auto-align-backslashes . nil)
   (c-basic-offset . ,tab-width)
   (c-offsets-alist
    (arglist-cont-nonempty . +)
    (arglist-intro . +)
    (c . 0)
    (case-label . 0)
    (cpp-define-intro . 0)
    (cpp-macro . 0)
    (knr-argdecl-intro . 0)
    (label . 0)
    (statement-block-intro . +)
    (statement-cont . +)
    (substatement-label . 0)
    (substatement-open . 0)
    )))

;; Note that in Emacs 24, cc-mode calls its hooks manually in each mode init
;; function. Since cc modes belong to prog-mode, each hook is called another
;; time at the end of the initialization. No big deal since we only set some
;; variables.
(mapcar
 (lambda (mode-hook)
   (add-hook-and-eval
    mode-hook
    (lambda ()
      ;; The cc-fmt hook is disable since there is no standard C formatting,
      ;; unlike for Go.
      ; (add-hook 'before-save-hook 'cc-fmt nil t)
      (c-set-style "peter") ;; We override existing values.
      (add-hook 'compilation-before-hook 'cc-set-compiler nil t)
      (local-set-key (kbd "<f9>") 'cc-clean)
      (local-set-key (kbd "M-TAB") 'semantic-complete-analyze-inline)
      (local-set-key (kbd "C-c (") 'cc-function)
      (local-set-key (kbd "C-c C-f") 'cc-for)
      (local-set-key (kbd "C-c C-i") 'cc-if)
      (local-set-key (kbd "C-c C-p") 'cc-printf)
      (local-set-key (kbd "C-c I") 'cc-include-local)
      (local-set-key (kbd "C-c i") 'cc-include)
      (local-set-key (kbd "C-c m") 'cc-main)
      ;; (local-set-key "." 'semantic-complete-self-insert) ; This is a bit slow.
      ;; (local-set-key ">" 'semantic-complete-self-insert)
      (local-set-key (kbd "C-M-e") (lambda () (interactive) (c-beginning-of-defun -1))))))
 '(c-mode-hook c++-mode-hook))

;;==============================================================================
;; Qt semantic support
;;==============================================================================

;; Qt base directory, meaning the directory where the 'Qt' directory can be found.
;; Adapt accordingly.
(when  (fboundp 'semantic-add-system-include)
  (setq qt4-base-dir "/usr/include/qt4")
  (setq qt4-gui-dir (concat qt4-base-dir "/QtGui"))
  (semantic-add-system-include qt4-base-dir 'c++-mode)
  (semantic-add-system-include qt4-gui-dir 'c++-mode)
  (add-to-list 'auto-mode-alist (cons qt4-base-dir 'c++-mode))
  (add-hook
   'c++-mode-hook
   (lambda ()
     (when semantic-mode
       (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qconfig.h"))
       (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qconfig-large.h"))
       (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qglobal.h"))))))

;;==============================================================================
;; Skel
;;==============================================================================

;; Note that it is possible to extend the skel syntax with
;; `skeleton-further-elements'. For instance:
; (setq skeleton-further-elements '((q "\"")))

(define-skeleton cc-case
  "Insert a case/switch statement."
  "expression: "
  > "switch (" str ") {" \n
  ( "Value, %s: "
    > "case " > str ":" \n
    "break;" \n)
  "default:" > \n
  > _ \n
  "break;" \n
  resume:
  "}" > \n)

(define-skeleton cc-debug
  "Insert debug macros."
  nil
  > "#ifdef DEBUG
#define DEBUG_CMD(CMD) do {CMD;} while(0)
#else
#define DEBUG_CMD(CMD) do {} while(0)
#endif

"
  '(insert-and-indent
    "#define DEBUG_STR(STR) DEBUG_CMD(fprintf(stderr, \":: %s\\n\", STR))
#define DEBUG_PRINT(...) DEBUG_CMD( \\
fprintf(stderr, \"\\n\\033[31;1mDEBUG:\\033[0m %s:%d:%s()\\t\", __FILE__, __LINE__, __func__); \\
fprintf(stderr, __VA_ARGS__); \\
fprintf(stderr, \"\\n\"); \\
)"))

(define-skeleton cc-for
  "for loop."
  nil
  '(setq str (skeleton-read "Variable: " "i"))
  > "for (" @ (skeleton-read "" (concat str " = 0")) "; "
  @ (skeleton-read "" (concat str " < N")) "; "
    @ (skeleton-read "" (concat str "++")) ") {" \n
      @ _ \n
        "}" > )

(define-skeleton cc-function
  "Insert punctuation for function."
  "Arguments: " '(just-one-space) "(" @ str ") {" \n
  > _ \n
  "}" > \n)

(define-skeleton cc-getopt
  "Insert a getopt template."
  nil
  > "int opt;" \n
  "while ((opt = getopt(argc, argv, \":hV\")) != -1) {" \n
  "switch(opt) {" \n
  "case 'h':" > \n
  "usage(argv[0]);" \n
  "return 0;" \n
  "case 'V':" > \n
  "version();" \n
  "return 0;" \n
  "case ':':" > \n
  "fprintf(stderr, \"ERROR: -%c needs an argument.\\nTry '%s -h' for more information.\\n\", optopt, argv[0]);" \n
  "return EXIT_FAILURE;" \n
  "case '?':" > \n
  "fprintf(stderr, \"ERROR: Unknown argument %c.\\nTry '%s -h' for more information.\\n\", optopt, argv[0]);" \n
  "return EXIT_FAILURE;" \n
  "default:" > \n
  "usage(argv[0]);" \n
  "return EXIT_SUCCESS;" \n
  "}" > \n
  "}" > "\n" \n
  "if (optind >= argc) {" \n
  "fprintf(stderr, \"Expected argument after options\\n\");" \n
  "exit(EXIT_FAILURE);" \n
  "}" > \n)

(define-skeleton cc-if
  "Insert an if statement."
  "Condition: "
  > "if (" @ str ") {" \n
  > @ _ \n
  ("Other condition, %s: "
   "} else if (" > @ str ") {" \n
   @ \n)
  "} else {" > \n
  @ \n
  resume:
  "}" > \n)

(define-skeleton cc-include
  "Insert system include."
  "Header: "
  \n "#include <" @ str ">" \n)

(define-skeleton cc-include-local
  "Insert local include."
  "Header: "
  \n "#include \"" @ str "\"" \n)

(define-skeleton cc-loadfile
  "Insert loadfile function."
  nil
  "unsigned long loadfile(const char *path, char **buffer_ptr) {" \n
  "#define MAX_FILESIZE 1073741824 /* One gigabyte */" > "\n" \n
  "/* Handle variable. */" \n
  "char *buffer;" "\n" \n
  "FILE *file = fopen(path, \"rb\");" \n
  "if (file == NULL) {" \n
  "perror(path);" \n
  "return 0;" \n
  "}" > "\n" \n
  "fseek(file, 0, SEEK_END);" \n
  "long length = ftell(file);" \n
  "/* fprintf(stdout, \"Note: file %s is %u bytes long.\\n\", path, length); */" "\n" \n
  "if (length > MAX_FILESIZE) {" \n
  "fprintf(stderr, \"%s size %ld is bigger than %d bytes.\\n\", path, length, MAX_FILESIZE);" \n
  "fclose(file);" \n
  "return 0;" \n
  "}" > "\n" \n
  "fseek(file, 0, SEEK_SET);" \n
  "buffer = (char *)malloc(length + 1);" \n
  "if (buffer == NULL) {" \n
  "perror(\"malloc\");" \n
  "fclose(file);" \n
  "return 0;" \n
  "}" > "\n" \n
  "if (fread(buffer, 1, length, file) == 0) {" \n
  "fclose(file);" \n
  "return 0;" \n
  "}" > "\n" \n
  "buffer[length] = '\\0';" \n
  "fclose(file);" "\n" \n
  "*buffer_ptr = buffer;" \n
  "return length;" \n
  "}" > \n)

(define-skeleton cc-main
  "Insert main function with basic includes."
  nil
  > "#include <inttypes.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

int main(int argc, char **argv) {" \n
> @ _ \n
> "return 0;
}" \n)

(define-skeleton cc-printf
  "fprintf/printf snippet.
If no file descriptor is provided, switch do printf.  The format
string is properly parsed (%% are not taken into account).\n
Requires `count-percents'."
  nil
  '(require 'functions)
  '(setq v1 (skeleton-read "File desc: " "stderr"))
  (if (string= v1 "") "printf(" (concat "fprintf(" v1 ", "))
  "\"" (setq v1 (skeleton-read "Format string: " "%s\\n")) "\""
  '(setq v2 (count-percents v1))
  '(setq v1 0)
  '(while (< v1 v2)
     (setq v1 (1+ v1))
     (skeleton-insert '(nil (concat ", " (skeleton-read "Value: ")))))
  @ ");")

(define-skeleton cc-usage-version
  "Insert usage() and version() functions."
  "Synopsis: "
  > "static void usage(const char *executable) {" \n
  "printf(\"Usage: %s [OPTIONS]\\n\\n\", executable);" \n
  "puts(\"" str "\\n\");" "\n" \n

  "puts(\"Options:\");" \n
  "puts(\"  -h        Print this help.\");" \n
  "puts(\"  -V        Print version information.\");" "\n" \n

  "puts(\"\");" \n
  "printf(\"See %s for more information.\\n\", MANPAGE);" \n
  "}" > "\n" \n

  "static void version() {" \n
  "printf(\"%s %s\\n\", APPNAME, VERSION);" \n
  "printf(\"Copyright © %s %s\\n\", YEAR, AUTHOR);" \n
  "}" > \n)

(provide 'mode-cc)