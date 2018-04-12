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

;; TODO: Use local dir instead.
;; TODO: Include sources.
(define-public emacs-prerelease
  (package
    (name "emacs-prerelease")
    (version "26.1-rc1")
    (source (origin
             (method url-fetch)
             (uri (string-append "ftp://alpha.gnu.org/gnu/emacs/pretest/emacs-" version ".tar.xz"))
             (sha256
              (base32
               "0n2pl1i4piga43p1kbscbb2sgg74gy4qq5jgmmrnxf80vrlfd535"))
             (patches (search-patches "emacs-exec-path.patch"
                                      ;; "emacs-fix-scheme-indent-function.patch"
                                      "emacs-source-date-epoch.patch"))
             (modules '((guix build utils)))
             (snippet
              ;; Delete the bundled byte-compiled elisp files and
              ;; generated autoloads.
              '(with-directory-excursion "lisp"
                 (for-each delete-file
                           (append (find-files "." "\\.elc$")
                                   (find-files "." "loaddefs\\.el$")
                                   ;; This is the only "autoloads" file that
                                   ;; does not have "*loaddefs.el" name.
                                   '("eshell/esh-groups.el")))

                 ;; Make sure Tramp looks for binaries in the right places on
                 ;; remote GuixSD machines, where 'getconf PATH' returns
                 ;; something bogus.
                 (substitute* "net/tramp-sh.el"
                   ;; Patch the line after "(defcustom tramp-remote-path".
                   (("\\(tramp-default-remote-path")
                    (format #f "(tramp-default-remote-path ~s ~s ~s ~s "
                            "~/.guix-profile/bin" "~/.guix-profile/sbin"
                            "/run/current-system/profile/bin"
                            "/run/current-system/profile/sbin")))

                 ;; Make sure Man looks for C header files in the right
                 ;; places.
                 (substitute* "man.el"
                   (("\"/usr/local/include\"" line)
                    (string-join
                     (list line
                           "\"~/.guix-profile/include\""
                           "\"/var/guix/profiles/system/profile/include\"")
                     " ")))))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-/bin/pwd
           (lambda _
             ;; Use `pwd', not `/bin/pwd'.
             (substitute* (find-files "." "^Makefile\\.in$")
               (("/bin/pwd")
                "pwd"))))
         (add-after 'install 'install-site-start
           ;; Use 'guix-emacs' in "site-start.el".  This way, Emacs packages
           ;; provided by Guix and installed in
           ;; ~/.guix-profile/share/emacs/site-lisp/guix.d/PACKAGE-VERSION are
           ;; automatically found.
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out      (assoc-ref outputs "out"))
                    (lisp-dir (string-append out "/share/emacs/site-lisp")))
               (copy-file (assoc-ref inputs "guix-emacs.el")
                          (string-append lisp-dir "/guix-emacs.el"))
               (with-output-to-file (string-append lisp-dir "/site-start.el")
                 (lambda ()
                   (display
                    (string-append "(when (require 'guix-emacs nil t)\n"
                                   "  (guix-emacs-autoload-packages))\n"))))
               #t))))))
    (inputs
     `(("gnutls" ,gnutls)
       ("ncurses" ,ncurses)

       ;; TODO: Add the optional dependencies.
       ("libx11" ,libx11)
       ("gtk+" ,gtk+)
       ("libxft" ,libxft)
       ("libtiff" ,libtiff)
       ("giflib" ,giflib)
       ("libjpeg" ,libjpeg-8)
       ("imagemagick" ,imagemagick)
       ("acl" ,acl)

       ;; When looking for libpng `configure' links with `-lpng -lz', so we
       ;; must also provide zlib as an input.
       ("libpng" ,libpng)
       ("zlib" ,zlib)

       ("librsvg" ,librsvg)
       ("libxpm" ,libxpm)
       ("libxml2" ,libxml2)
       ("libice" ,libice)
       ("libsm" ,libsm)
       ("alsa-lib" ,alsa-lib)
       ("dbus" ,dbus)

       ;; multilingualization support
       ("libotf" ,libotf)
       ("m17n-lib" ,m17n-lib)))
    (native-inputs
     `(("guix-emacs.el" ,(search-auxiliary-file "emacs/guix-emacs.el"))
       ("pkg-config" ,pkg-config)
       ("texinfo" ,texinfo)))

    (native-search-paths
     (list (search-path-specification
            (variable "INFOPATH")
            (files '("share/info")))))

    (home-page "https://www.gnu.org/software/emacs/")
    (synopsis "The extensible, customizable, self-documenting text editor")
    (description
     "GNU Emacs is an extensible and highly customizable text editor.  It is
based on an Emacs Lisp interpreter with extensions for text editing.  Emacs
has been extended in essentially all areas of computing, giving rise to a
vast array of packages supporting, e.g., email, IRC and XMPP messaging,
spreadsheets, remote server editing, and much more.  Emacs includes extensive
documentation on all aspects of the system, from basic editing to writing
large Lisp programs.  It has full Unicode support for nearly all human
languages.")
    (license license:gpl3+)))
