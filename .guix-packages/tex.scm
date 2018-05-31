(define-module (tex)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages perl)
  #:use-module (guix build-system texlive)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix svn-download)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module ((srfi srfi-1) #:hide (zip)))

(define-public texlive-generic-ulem
  (package
    (name "texlive-generic-ulem")
    (version (number->string %texlive-revision))
    (source
     (origin
       (method svn-fetch)
       (uri (svn-reference
             (url (string-append "svn://www.tug.org/texlive/tags/"
                                 %texlive-tag "/Master/texmf-dist/"
                                 "/tex/generic/ulem"))
             (revision %texlive-revision)))
       (file-name (string-append name "-" version "-checkout"))
       (sha256
        (base32
         "1rzdniqq9zk39w8ch8ylx3ywh2mj87s4ivchrsk2b8nx06jyn797"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((target (string-append (assoc-ref %outputs "out")
                                      "/share/texmf-dist/tex/generic/ulem")))
           (mkdir-p target)
           (copy-recursively (assoc-ref %build-inputs "source") target)
           #t))))
    (home-page "https://www.ctan.org/pkg/ulem")
    (synopsis "Underline text")
    (description
     "The package provides an @code{\\ul} (underline) command which will break
over line ends; this technique may be used to replace @code{\\em} (both in that
form and as the @code{\\emph} command), so as to make output look as if it comes
from a typewriter. The package also offers double and wavy underlining, and
striking out (line through words) and crossing out (/// over words).")
    ;; TODO: Check if this is the proper license.
    (license license:lppl1.3c+)))

(define-public texlive-latex-pgf
  (package
    (name "texlive-latex-pgf")
    (version (number->string %texlive-revision))
    (source
     (origin
       (method svn-fetch)
       (uri (svn-reference
             (url (string-append "svn://www.tug.org/texlive/tags/"
                                 %texlive-tag "/Master/texmf-dist/"
                                 "/tex/latex/pgf"))
             (revision %texlive-revision)))
       (file-name (string-append name "-" version "-checkout"))
       (sha256
        (base32
         "1dq8p10pz8wn0vx412m7d7d5gj1syxly3yqdqvf7lv2xl8zndn5h"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((target (string-append (assoc-ref %outputs "out")
                                      "/share/texmf-dist/tex/latex/pgf")))
           (mkdir-p target)
           (copy-recursively (assoc-ref %build-inputs "source") target)
           #t))))
    (home-page "https://www.ctan.org/pkg/tikz")
    (synopsis "Create PostScript and PDF graphics in TeX")
    (description
     "PGF is a macro package for creating graphics.  It is platform- and
format-independent and works together with the most important TeX backend
drivers, including pdfTeX and dvips.  It comes with a user-friendly syntax layer
called TikZ.

Its usage is similar to pstricks and the standard picture environment. PGF works
with plain (pdf-)TeX, (pdf-)LaTeX, and ConTeXt. Unlike pstricks, it can produce
either PostScript or PDF output.")
    (license license:lppl1.3c+)))

;; TODO: Koma-script does not build.
;; Try pre-built file https://aur.archlinux.org/cgit/aur.git/tree/PKGBUILD?h=koma-script.
(define-public texlive-latex-koma-script
  (package
    (name "texlive-latex-koma-script")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "koma-script"))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0nn0nr437f5rasmb48iq2h2gvmvpafkayihx7wvxkfh7rrr8ils4"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("texlive-bin" ,texlive-bin)
       ("perl" ,perl)
       ;; ("texlive-metafont-base" ,texlive-metafont-base)
       ;; ("texlive-fonts-cm" ,texlive-fonts-cm)
       ))
    ;; (arguments
    ;;  `(#:modules ((guix build utils))
    ;;    #:builder
    ;;    (begin
    ;;      (use-modules (guix build utils))
    ;;      (let ((target (string-append (assoc-ref %outputs "out")
    ;;                                   "/share/texmf-dist/tex/latex/eukdate")))
    ;;        (mkdir-p target)
    ;;        (copy-recursively (assoc-ref %build-inputs "source") target)
    ;;        #t))))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; TODO: Need to set PATH variable in Makefile.baseinit.
         (delete 'configure))))
    (inputs
     `(("texlive-latex-filecontents" ,texlive-latex-filecontents)
       ("texlive-latex-babel" ,texlive-latex-babel)))
    (home-page "https://www.ctan.org/pkg/koma-script")
    (synopsis "Bundle of versatile classes and packages")
    (description
     "The KOMA-Script bundle provides replacements for the article, report, and
book classes with emphasis on typography and versatility.  There is also a
letter class.

The bundle also offers:

@itemize
@item a package for calculating type areas in the way laid down by the
typographer Jan Tschichold,
@item packages for easily changing and defining page styles,
@item a package scrdate for getting not only the current date but also the name
of the day, and
@item a package scrtime for getting the current time.
@end itemize

All these packages may be used not only with KOMA-Script classes but also with
the standard classes.

Since every package has its own version number, the version number quoted only
refers to the version of scrbook, scrreprt, scrartcl, scrlttr2 and
typearea (which are the main parts of the bundle).")
    (license license:lppl1.3+)))

;; TODO: Marvosym fonts build but they can't be used properly.
;; http://www.tug.org/svn/texlive/tags/texlive-2017.1/Master/texmf-dist/source/fonts/marvosym/
;; https://github.com/mojca/marvosym
;; TODO: Check out texlive-fonts-ec, we need to patch some PATHS.
(define-public texlive-fonts-marvosym
  (package
    (name "texlive-fonts-marvosym")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url (string-append "svn://www.tug.org/texlive/tags/"
                                        %texlive-tag "/Master/texmf-dist/"
                                        "/tex/latex/marvosym"))
                    (revision %texlive-revision)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0qhrccc340ipmq93jh8dmpai81dk46wi34impd6lrzx72fi17s7g"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils)
                  (ice-9 match))
       #:builder
       (begin
         (use-modules (guix build utils)
                      (ice-9 match))
         (let ((root (string-append (assoc-ref %outputs "out")
                                    "/share/texmf-dist/"))
               (pkgs '(("source"        . "tex/latex/marvosym")
                       ("marvosym-truetype"    . "fonts/truetype/public/marvosym")
                       ("marvosym-afm"   . "fonts/afm/public/marvosym")
                       ("marvosym-tfm"   . "fonts/tfm/public/marvosym")
                       ("marvosym-type1" . "fonts/type1/public/marvosym")
                       ;; ("marvosym-enc"   . "fonts/enc/dvips/marvosym")
                       ("marvosym-map"   . "fonts/map/dvips/marvosym"))))
           (for-each (match-lambda
                       ((pkg . dir)
                        (let ((target (string-append root dir)))
                          (mkdir-p target)
                          (copy-recursively (assoc-ref %build-inputs pkg)
                                            target))))
                     pkgs)
           #t))))
    (native-inputs
     `(("marvosym-tfm"
        ,(origin
           (method svn-fetch)
           (uri (svn-reference
                 (url (string-append "svn://www.tug.org/texlive/tags/"
                                     %texlive-tag "/Master/texmf-dist/"
                                     "/fonts/tfm/public/marvosym"))
                 (revision %texlive-revision)))
           (file-name (string-append name "-tfm-" version "-checkout"))
           (sha256
            (base32
             "1912j5p59baij47cr793jsjsp465077g990iif6vm6bgg7ha8b2v"))))
       ("marvosym-afm"
        ,(origin
           (method svn-fetch)
           (uri (svn-reference
                 (url (string-append "svn://www.tug.org/texlive/tags/"
                                     %texlive-tag "/Master/texmf-dist/"
                                     "/fonts/afm/public/marvosym"))
                 (revision %texlive-revision)))
           (file-name (string-append name "-afm-" version "-checkout"))
           (sha256
            (base32
             "09jb393ivgnk4brx8jgll5dpfx2hqb2h94i94lqv30snbnhw93k8"))))
       ("marvosym-type1"
        ,(origin
           (method svn-fetch)
           (uri (svn-reference
                 (url (string-append "svn://www.tug.org/texlive/tags/"
                                     %texlive-tag "/Master/texmf-dist/"
                                     "/fonts/type1/public/marvosym"))
                 (revision %texlive-revision)))
           (file-name (string-append name "-type1-" version "-checkout"))
           (sha256
            (base32
             "19kri8lf2z6j3b74iczppj01j28m3v2qbwq9507nxswfjxxlmb22"))))
       ("marvosym-truetype"
        ,(origin
           (method svn-fetch)
           (uri (svn-reference
                 (url (string-append "svn://www.tug.org/texlive/tags/"
                                     %texlive-tag "/Master/texmf-dist/"
                                     "/fonts/truetype/public/marvosym"))
                 (revision %texlive-revision)))
           (file-name (string-append name "-truetype-" version "-checkout"))
           (sha256
            (base32
             "1x4yrpwwjfvhfvcby9w4dv0kdsgz0ic0c0i5zg1h692grvc0rzar"))))
       ("marvosym-map"
        ,(origin
           (method svn-fetch)
           (uri (svn-reference
                 (url (string-append "svn://www.tug.org/texlive/tags/"
                                     %texlive-tag "/Master/texmf-dist/"
                                     "/fonts/map/dvips/marvosym"))
                 (revision %texlive-revision)))
           (file-name (string-append name "-map-" version "-checkout"))
           (sha256
            (base32
             "1ybwqpwmr79ma9sf0c7apyadhldzsxbwbqgnracaiy810mjzychf"))))))
    (home-page "https://www.ctan.org/pkg/threeparttable")
    (synopsis "Martin Vogel's Symbols (marvosym) font")
    (description
     "Martin Vogel’s Symbol font (marvosym) contains the Euro currency symbol as
;; defined by the European commission, along with symbols for structural
;; engineering; symbols for steel cross-sections; astronomy signs (sun, moon,
;; planets); the 12 signs of the zodiac; scissor symbols; CE sign and others.

;; The package contains both the original TrueType font and the derived Type 1
;; font, together with support files for TeX (LaTeX).")
    ;; TODO: Fix license.
    (license license:gpl3+)))

(define-public texlive-latex-eukdate
  (package
    (name "texlive-latex-eukdate")
    (version (number->string %texlive-revision))
    (source
     (origin
       (method svn-fetch)
       (uri (svn-reference
             (url (string-append "svn://www.tug.org/texlive/tags/"
                                 %texlive-tag "/Master/texmf-dist/"
                                 "/tex/latex/eukdate"))
             (revision %texlive-revision)))
       (file-name (string-append name "-" version "-checkout"))
       (sha256
        (base32
         "18xan116l8w47v560bkw6nbhkrml7g04xrlzk3jrpc7qsyf3n5fz"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((target (string-append (assoc-ref %outputs "out")
                                      "/share/texmf-dist/tex/latex/eukdate")))
           (mkdir-p target)
           (copy-recursively (assoc-ref %build-inputs "source") target)
           #t))))
    (home-page "https://www.ctan.org/pkg/eukdate")
    (synopsis "UK format dates, with weekday")
    (description
     "The package is used to change the format of @code{\\today}’s date,
including the weekday, e.g., \"Saturday, 26 June 2008\", the 'UK format', which
is preferred in many parts of the world, as distinct from that which is used in
@code{\\maketitle} of the article class, \"June 26, 2008\", the 'US format'.")
    (license license:lppl1.3c+)))

(define-public texlive-latex-needspace
  (package
    (name "texlive-latex-needspace")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "needspace"))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0kw80f5jh4gdpa2ka815abza3gr5z8b929w0745vrlc59pl0017y"))))
    (build-system texlive-build-system)
    (arguments
     '(#:tex-directory "latex/needspace"
       #:tex-format "latex"))
    (inputs
     `(("texlive-latex-filecontents" ,texlive-latex-filecontents)))
    ;; (build-system trivial-build-system)
    ;; (arguments
    ;;  `(#:modules ((guix build utils))
    ;;    #:builder
    ;;    (begin
    ;;      (use-modules (guix build utils))
    ;;      (let ((target (string-append (assoc-ref %outputs "out")
    ;;                                   "/share/texmf-dist/tex/latex/needspace")))
    ;;        (mkdir-p target)
    ;;        (copy-recursively (assoc-ref %build-inputs "source") target)
    ;;        #t))))
    (home-page "https://www.ctan.org/pkg/needspace")
    (synopsis "Insert pagebreak if not enough space")
    (description
     "Provides commands to disable pagebreaking within a given vertical
space. If there is not enough space between the command and the bottom of the
page, a new page will be started.")
    ;; TODO: Check if this is the proper license.
    (license license:lppl1.3c+)))

;; TODO: Do I need microtype?
;; (define-public texlive-latex-microtype
;;   (package
;;     (name "texlive-latex-microtype")
;;     (version (number->string %texlive-revision))
;;     (source (origin
;;               (method svn-fetch)
;;               (uri (texlive-ref "latex" "microtype"))
;;               (file-name (string-append name "-" version "-checkout"))
;;               (sha256
;;                (base32
;;                 "1dwhlxy35bydlljb40ck6d5j93gd4889hpr4j3x8vqhl46k3lfph"))))
;;     (build-system texlive-build-system)
;;     (arguments '(#:tex-directory "latex/microtype"))
;;     ;; (arguments
;;     ;;  `(#:modules ((guix build utils))
;;     ;;    #:builder
;;     ;;    (begin
;;     ;;      (use-modules (guix build utils))
;;     ;;      (let ((target (string-append (assoc-ref %outputs "out")
;;     ;;                                   "/share/texmf-dist/tex/latex/needspace")))
;;     ;;        (mkdir-p target)
;;     ;;        (copy-recursively (assoc-ref %build-inputs "source") target)
;;     ;;        #t))))
;;     (home-page "https://www.ctan.org/pkg/microtype")
;;     (synopsis "Subliminal refinements towards typographical perfection")
;;     (description
;;      "The package provides a LaTeX interface to the micro-typographic
;; extensions that were introduced by pdfTeX and have since also propagated to
;; XeTeX and LuaTeX: most prominently, character protrusion and font expansion,
;; furthermore the adjustment of interword spacing and additional kerning, as well
;; as hyphenatable letterspacing (tracking) and the possibility to disable all or
;; selected ligatures.

;; These features may be applied to customisable sets of fonts, and all
;; micro-typographic aspects of the fonts can be configured in a straight-forward
;; and flexible way. Settings for various fonts are provided.

;; Note that character protrusion requires pdfTeX, LuaTeX, or XeTeX. Font expansion
;; works with pdfTeX or LuaTeX. The package will by default enable protrusion and
;; expansion if they can safely be assumed to work. Disabling ligatures requires
;; pdfTeX or LuaTeX, while the adjustment of interword spacing and of kerning only
;; works with pdfTeX. Letterspacing is available with pdfTeX or LuaTeX.

;; The alternative package @code{letterspace}, which also works with plain TeX, provides
;; the user commands for letterspacing only, omitting support for all other
;; extensions.
;; ")
;;     ;; TODO: Check if this is the proper license.
;;     (license license:lppl1.3c+)))

;; TODO: Rename texlive-for-org-letter
(define-public texlive-medium
  (package
    (inherit (texlive-union
              (list
               texlive-latex-oberdiek
               texlive-generic-ifxetex
               texlive-latex-wrapfig
               texlive-fonts-amsfonts   ; For custom letter?
               ; texlive-dvips   ; For custom letter and marvosym?  Already in texlive-union?
               ;; texlive-latex-amscls
               texlive-latex-amsfonts
               ;; texlive-latex-amsmath
               ;; texlive-latex-amsrefs
               texlive-latex-capt-of
               texlive-latex-hyperref
               texlive-latex-url
               ;; TODO: For custom letter
               texlive-fonts-ec
               texlive-latex-geometry
               texlive-latex-xcolor
               ;; TODO: For custom letter + send patches for those.
               texlive-fonts-marvosym
               texlive-latex-eukdate
               texlive-latex-needspace
               ;; TODO: For needspace (build inputs)
               texlive-latex-filecontents
               texlive-latex-titlesec
               ;; TODO: needspace needs microtype?
               ;; TODO: Send patches for those.
               texlive-latex-pgf
               texlive-generic-ulem)))
    (name "texlive-medium")
    (description "This is a very limited subset of the TeX Live distribution.
It includes little more than texlive-tiny.")))
