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

;; TODO: Include sources.
;; TODO: Make a script which calls
;; > Patch s,/bin/pwd,pwd,g in all Makefile.in files.
;; > guix environment emacs -- ./configure --prefix="/gnu/store/n6cr7jvmj8j4ig3ldkzsrnywmlilm1ap-emacs-dev-27.0.0" --localstatedir=/var
;; > guix environment emacs -- make DESTDIR=$HOME/projects/emacs/output -j5 install
;; (Full path to output is important.)
;; TODO: Glib wrapper as in original Guix package?
;; #!/gnu/store/mm0zclrzj3y7rj74hzyd0f224xly04fh-bash-minimal-4.4.12/bin/bash
;; export XDG_DATA_DIRS="/gnu/store/jj58sgi6aa73s1k09x7ximrspnii5q0n-shared-mime-info-1.8/share:/gnu/store/i1x64fz6m3i1l61vywn5bgdamw0h71c0-glib-2.54.2/share:/gnu/store/dvzhbixz5brkabisgq2m6i7yj4543c1y-gtk+-3.22.29/share:/gnu/store/0ffgqq2x44f09qnlck62m354kfqznw83-emacs-prerelease-26.1-rc1/share${XDG_DATA_DIRS:+:}$XDG_DATA_DIRS"
;; export GTK_PATH="/gnu/store/dvzhbixz5brkabisgq2m6i7yj4543c1y-gtk+-3.22.29/lib/gtk-3.0${GTK_PATH:+:}$GTK_PATH"
;; export GIO_EXTRA_MODULES="/gnu/store/i1x64fz6m3i1l61vywn5bgdamw0h71c0-glib-2.54.2/lib/gio/modules${GIO_EXTRA_MODULES:+:}$GIO_EXTRA_MODULES"
;; exec -a "$0" "/gnu/store/0ffgqq2x44f09qnlck62m354kfqznw83-emacs-prerelease-26.1-rc1/bin/emacs-26.1" "$@"
;; Try:
;; export XDG_DATA_DIRS="/gnu/store/n6cr7jvmj8j4ig3ldkzsrnywmlilm1ap-emacs-dev-27.0.0${XDG_DATA_DIRS:+:}$XDG_DATA_DIRS"
(define-public emacs-dev
  (package
    (inherit emacs)
    (name "emacs-dev")
    (version "27.0.0")                  ; TODO: Use git version?
    (source (local-file (string-append (getenv "HOME") "/projects/emacs/output")
                        #:recursive? #t))
    ;; TODO: Include patches and snippets?
    ;; - emacs-exec-path.patch is useless because we don't need our build to be reproduicible.
    ;; - emacs-source-date-epoch.patch: same.
    ;; - emacs-fix-scheme-indent-function.patch?
    ;; - Snippet to delete .elc and loadefs?  Probably not.
    ;; - Snippet tramp-default-remote-path?
    ;; - Snippet for C header path in man.el?
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (copy-recursively (assoc-ref %build-inputs "source") %output)
         ;; Install guix-emacs as per original Emacs package declaration.
         (let* ((out %output)
                (lisp-dir (string-append out "/share/emacs/site-lisp")))
           (copy-file (assoc-ref %build-inputs "guix-emacs.el")
                      (string-append lisp-dir "/guix-emacs.el"))
           (with-output-to-file (string-append lisp-dir "/site-start.el")
             (lambda ()
               (display
                (string-append "(when (require 'guix-emacs nil t)\n"
                               "  (guix-emacs-autoload-packages))\n")))))
         #t)))
    (synopsis "Emacs (development version)")
    (license license:lgpl3+)))
