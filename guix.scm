(use-modules
  ((guix licenses) #:prefix license:)
  (gnu packages autotools)
  (gnu packages gnupg)
  (gnu packages guile)
  (gnu packages guile-xyz)
  (gnu packages pkg-config)
  (gnu packages texinfo)
  (gnu packages)
  (guix build-system gnu)
  (guix download)
  (guix gexp)
  (guix packages)
  (srfi srfi-1))

(package
  (name "guile-neocities")
  (version "0.1")
  (source
    (local-file
      (dirname (current-filename))
      #:recursive? #t
      #:select? (lambda (file stat)
                  (not (any (lambda (my-string)
                              (string-contains file my-string))
                            (list ".git" "guix.scm"))))))
  (build-system gnu-build-system)
  (arguments
    `(#:modules
      ((ice-9 match)
       (ice-9 ftw)
       ,@%gnu-build-system-modules)
      #:phases
      (modify-phases
        %standard-phases
        (add-after
          'install
          'hall-wrap-binaries
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let* ((compiled-dir
                     (lambda (out version)
                       (string-append
                         out
                         "/lib/guile/"
                         version
                         "/site-ccache")))
                   (uncompiled-dir
                     (lambda (out version)
                       (string-append
                         out
                         "/share/guile/site"
                         (if (string-null? version) "" "/")
                         version)))
                   (dep-path
                     (lambda (env modules path)
                       (list env
                             ":"
                             'prefix
                             (cons modules
                                   (map (lambda (input)
                                          (string-append
                                            (assoc-ref inputs input)
                                            path))
                                        ,''("guile-json" "guile-gcrypt"))))))
                   (out (assoc-ref outputs "out"))
                   (bin (string-append out "/bin/"))
                   (site (uncompiled-dir out "")))
              (match (scandir site)
                     (("." ".." version)
                      (for-each
                        (lambda (file)
                          (wrap-program
                            (string-append bin file)
                            (dep-path
                              "GUILE_LOAD_PATH"
                              (uncompiled-dir out version)
                              (uncompiled-dir "" version))
                            (dep-path
                              "GUILE_LOAD_COMPILED_PATH"
                              (compiled-dir out version)
                              (compiled-dir "" version))))
                        ,''("neocities"))
                      #t))))))))
  (native-inputs (list autoconf automake pkg-config texinfo))
  (inputs (list guile-3.0))
  (propagated-inputs (list guile-json-4 guile-gcrypt))
  (synopsis "Neocities API and Command Line tool")
  (description
    "Command line tool and API access for Neocities.org")
  (home-page "http://git.elenq.tech/guile-neocities/")
  (license license:gpl3+))

