(define-module (emacs)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages emacs)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system emacs)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages code)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages python)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages lesstif)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages w3m)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages scheme)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages fontutils)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match))

(define-public emacs-dev
  (package
   (inherit emacs)
   (name "emacs-dev")
   (version "26.0.91")
   (source
    ;; (local-file "/home/ambrevar/projects/emacs-build")
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "/home/ambrevar/projects/emacs")
           (commit "")))
     (sha256
      (base32
       "1b7n3g4m2rbvrwsgbfl8wl91z42g1ld42clwxs8qpl9ny5rwz6sq"))
     (patches (search-patches "emacs-exec-path.patch"
                              ;; "emacs-fix-scheme-indent-function.patch"
                              "emacs-source-date-epoch.patch"))
     (modules '((guix build utils)))
     (snippet
      ;; Delete the bundled byte-compiled elisp files and
      ;; generated autoloads.
      (quote (with-directory-excursion
              "lisp"
              (for-each delete-file
                        (append (find-files "." "\\.elc$")
                                (find-files "." "loaddefs\\.el$")
                                ;; This is the only "autoloads" file that
                                ;; does not have "*loaddefs.el" name.
                                (quote ("eshell/esh-groups.el"))))
              ;; Make sure Tramp looks for binaries in the right places on
              ;; remote GuixSD machines, where 'getconf PATH' returns
              ;; something bogus.
              (substitute*
               "net/tramp-sh.el"
               ;; Patch the line after "(defcustom tramp-remote-path".
               (("\\(tramp-default-remote-path")
                (format #f "(tramp-default-remote-path ~s ~s ~s ~s "
                        "~/.guix-profile/bin" "~/.guix-profile/sbin"
                        "/run/current-system/profile/bin"
                        "/run/current-system/profile/sbin")))
              ;; Make sure Man looks for C header files in the right
              ;; places.
              (substitute*
               "man.el"
               (("\"/usr/local/include\"" line)
                (string-join
                 (list line
                       "\"~/.guix-profile/include\""
                       "\"/var/guix/profiles/system/profile/include\"")
                 " ")))))))
    )))
