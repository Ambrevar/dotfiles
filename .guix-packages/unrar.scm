(define-module (unrar)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix download))

(define-public unrar
  (package
    (name "unrar")
    (version "5.6.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.rarlab.com/rar/unrarsrc-"
                                  version
                                  ".tar.gz"))
              (sha256
               (base32
                "1zj9pbk38qf7anfk52xq5j9j7p7i007fhjy00x007wkh30hd4dck"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:make-flags (list (string-append "DESTDIR=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (home-page "http://www.rarlab.com/rar_add.htm")
    (synopsis "The RAR uncompression program")
    (description "The RAR uncompression program.")
    (license ((@@ (guix licenses) license) "RARlab Copyright"
              "http://www.rarlab.com"
              ""))))
