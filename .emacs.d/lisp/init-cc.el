;;; C/C++

;;; TODO: Should we split this into mode-c and mode-c++?

(dolist (map (list c-mode-map c++-mode-map))
  (ambrevar/define-keys map "C-c m" 'cc-main
                        "<f5>" 'ambrevar/cc-clean
                        "M-." 'semantic-ia-fast-jump
                        "C-c C-d" 'semantic-ia-show-summary
                        "M-<tab>" 'semantic-complete-analyze-inline)
  (when (require 'company nil t)
    (define-key map (kbd "M-<tab>") (if (require 'helm-company nil t) 'helm-company 'company-complete))))
;; (define-key map (kbd "C-c o") 'ff-find-other-file)

(defvaralias 'c-basic-offset 'tab-width)

;;; C additional faces.
;;; Useless in quasi-monochrome.
;; (dolist (mode '(c-mode c++-mode))
;;   (font-lock-add-keywords
;;    mode
;;    '(("\\<\\(and\\|or\\|not\\)\\>" . font-lock-keyword-face)
;;      ;; Functions.
;;      ("\\<\\(\\sw+\\)(" 1 'font-lock-function-name-face)
;;      ("\\<\\(\\sw+\\)<\\sw+>(" 1 'font-lock-function-name-face))))

(defvar-local ambrevar/cc-ldlibs "-lm -pthread"
  "Custom linker flags for C/C++ linkage.")

(defvar-local ambrevar/cc-ldflags ""
  "Custom linker libs for C/C++ linkage.")

(defun ambrevar/cc-set-compiler (&optional nomakefile)
  "Set compile command to be nearest Makefile or a generic command.
The Makefile is looked up in parent folders. If no Makefile is
found (or if NOMAKEFILE is non-nil or if function was called with
universal argument), then a configurable commandline is
provided."
  (interactive "P")
  (hack-local-variables)
  ;; Alternatively, if a Makefile is found, we could change default directory
  ;; and leave the compile command to "make".  Changing `default-directory'
  ;; could have side effects though.
  (let ((makefile-dir (locate-dominating-file "." "Makefile")))
    (if (and makefile-dir (not nomakefile))
        (setq compile-command (concat "make -k -C " (shell-quote-argument (file-name-directory makefile-dir))))
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
                      (or (getenv "LDFLAGS") ambrevar/cc-ldflags)
                      (or (getenv "LDLIBS") ambrevar/cc-ldlibs)))))))

(defun ambrevar/cc-clean ()
  "Find Makefile and call the `clean' rule. If no Makefile is
found, no action is taken. The previous `compile' command is
restored."
  (interactive)
  (let (compile-command
        (makefile-dir (locate-dominating-file "." "Makefile")))
    (when makefile-dir
      (compile (format "make -k -C %s clean" (shell-quote-argument makefile-dir))))))

;;; It is tempting to add `ambrevar/cc-fmt' to the hook:
;; (add-hook 'before-save-hook 'ambrevar/cc-prettify nil t)
;;; Unlike Go however, there is no formatting standard and thus this would break
;;; the formatting rules of every third-party C file that does not follow the
;;; same style.
(defun ambrevar/cc-prettify ()
  "Run uncrustify(1) on current buffer or region."
  (interactive)
  (let  (status
         start end
         (formatbuf (get-buffer-create "*C format buffer*")))
    (if (use-region-p)
        (setq start (region-beginning) end (region-end))
      (setq start (point-min) end (point-max)))
    (setq status
          (call-process-region start end "uncrustify" nil formatbuf nil "-lc" "-q" "-c" (expand-file-name ".uncrustify.cfg" (getenv "HOME"))))
    (if (/= status 0)
        (error "error running uncrustify")
      (delete-region start end)
      (insert-buffer-substring formatbuf)
      (kill-buffer formatbuf))))

;;; GMP documentation
(with-eval-after-load "info-look"
  (let ((mode-value (assoc 'c-mode (assoc 'symbol info-lookup-alist))))
    (setcar (nthcdr 3 mode-value)
            (cons '("(gmp)Function Index" nil "^ -.* " "\\>")
                  (nth 3 mode-value)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Options

;;; Make sure Semanticdb folders is set before starting semantic.
(semantic-mode 1)

;;; Extra semantic support
;;; Example:
;; (when (fboundp 'semantic-add-system-include)
;;   (semantic-add-system-include "new/header/dir" 'c++-mode)
;;   (add-to-list 'auto-mode-alist (cons qt4-base-dir 'c++-mode))
;;   (add-hook
;;    'c++-mode-hook
;;    (lambda ()
;;      (when semantic-mode
;;        (add-to-list 'semantic-lex-c-preprocessor-symbol-file "new/header/dir/config.h")))))

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
    (substatement-open . 0))))
(nconc c-default-style '((c-mode . "ambrevar") (c++-mode . "ambrevar")))

;;; Note that in Emacs 24, cc-mode calls its hooks manually in each mode init
;;; function. Since cc modes belong to prog-mode, each hook is called another
;;; time at the end of the initialization. No big deal since we only set some
;;; variables.
(dolist (hook '(c-mode-hook c++-mode-hook))
  (when (require 'company nil t)
    (add-hook hook 'company-mode))
  (add-hook hook 'ambrevar/cc-set-compiler))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Skeletons

;;; Note that it is possible to extend the skel syntax with
;;; `skeleton-further-elements'. For instance:
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

(provide 'init-cc)
