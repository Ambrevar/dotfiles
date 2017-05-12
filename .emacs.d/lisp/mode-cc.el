;; C/C++

;; Should we split this in mode-c and mode-c++?

(defvar-local cc-ldlibs "-lm -pthread"
  "Custom linker flags for C/C++ linkage.")

(defvar-local cc-ldflags ""
  "Custom linker libs for C/C++ linkage.")

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
    (if (and makefile (not nomakefile))
        (setq compile-command (concat "make -k -C " (shell-quote-argument (file-name-directory makefile))))
      (setq compile-command
           (let
               ((c++-p (eq major-mode 'c++-mode))
                (file (file-name-nondirectory buffer-file-name)))
             (format "%s %s -o '%s' %s %s %s"
                     (if c++-p
                         (or (getenv "CXX") "g++")
                       (or (getenv "CC") "gcc"))
                     (shell-quote-argument file)
                     (shell-quote-argument (file-name-sans-extension file))
                     (if c++-p
                         (or (getenv "CXXFLAGS") "-Wall -Wextra -Wshadow -DDEBUG=9 -g3 -O0")
                       (or (getenv "CFLAGS") "-ansi -pedantic -std=c11 -Wall -Wextra -Wshadow -DDEBUG=9 -g3 -O0"))
                     (or (getenv "LDFLAGS") cc-ldflags)
                     (or (getenv "LDLIBS") cc-ldlibs)))))))

(defun cc-clean ()
  "Find Makefile and call the `clean' rule. If no Makefile is
found, no action is taken. The previous `compile' command is then
restored."
  (interactive)
  (let (compile-command (makefile (get-closest-pathname)))
    (when makefile
      (compile (format "make -k -f '%s' clean" makefile)))))

(defun cc-fmt ()
  "Run uncrustify(1) on current buffer or region."
  (interactive)
  (unless mark-active
    (mark-whole-buffer))
  (when (> (point) (mark))
    (exchange-point-and-mark))
  (let ((status)
        (formatbuf (get-buffer-create "*C format buffer*")))
    (setq status
          (call-process-region (point) (mark) "uncrustify" nil formatbuf nil "-lc" "-q" "-c" (concat (getenv "HOME") "/.uncrustify.cfg")))
    (if (/= status 0)
        (error "error running uncrustify")
      (delete-region (point) (mark))
      (insert-buffer formatbuf)
      (kill-buffer formatbuf))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Semantic options.

;; Semanticdb folders must be set before starting semantic.
(setq semanticdb-default-save-directory (concat emacs-cache-folder "semanticdb"))
(semantic-mode 1)

;; Extra semantic support
;; Example:
; (when  (fboundp 'semantic-add-system-include)
;   (semantic-add-system-include "new/header/dir" 'c++-mode)
;   (add-to-list 'auto-mode-alist (cons qt4-base-dir 'c++-mode))
;   (add-hook
;    'c++-mode-hook
;    (lambda ()
;      (when semantic-mode
;        (add-to-list 'semantic-lex-c-preprocessor-symbol-file "new/header/dir/config.h")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C-mode

;; WARNING: this style is a work-in-progress.
(c-add-style
 "ambrevar"
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
      (c-set-style "ambrevar") ;; We override existing values.
      (cc-set-compiler)
      (local-set-key (kbd "<f9>") 'cc-clean)
      (local-set-key (kbd "M-.") 'semantic-ia-fast-jump)
      (local-set-key (kbd "C-c C-d") 'semantic-ia-show-summary)
      (local-set-key (kbd "M-TAB") 'semantic-complete-analyze-inline)
      (when (fboundp 'company-mode)
        (company-mode)
        (if (fboundp 'helm-company)
            (local-set-key (kbd "M-TAB") 'helm-company)
          (local-set-key (kbd "M-TAB") 'company-complete)))
      (local-set-key (kbd "C-c o") 'ff-find-other-file)
      (local-set-key (kbd "C-c m") 'cc-main)
      ;; The cc-fmt hook is disable since there is no standard C formatting,
      ;; unlike for Go.
      ; (add-hook 'before-save-hook 'cc-fmt nil t)
      (local-set-key (kbd "C-M-e") (lambda () (interactive) (c-beginning-of-defun -1))))))
 '(c-mode-hook c++-mode-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Skeletons

;; Note that it is possible to extend the skel syntax with
;; `skeleton-further-elements'. For instance:
; (setq skeleton-further-elements '((q "\"")))

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
    "#define DEBUG_PRINT(...) DEBUG_CMD( \\
fprintf(stderr, \"%s:%d:\\t(%s)\\t\", __FILE__, __LINE__, __func__); \\
fprintf(stderr, __VA_ARGS__); \\
fprintf(stderr, \"\\n\"); \\
)"))

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
