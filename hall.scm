(hall-description
  (name "neocities")
  (prefix "guile")
  (version "0.0.1")
  (author "Ekaitz Zarraga")
  (email "")
  (copyright (2023))
  (synopsis "Neocities API and Command Line tool")
  (description
    "Command line tool and API access for Neocities.org")
  (home-page "http://git.elenq.tech/guile-neocities/")
  (license gpl3+)
  (dependencies
    `(("guile-json" (json) ,guile-json-4)
      ("guile-gcrypt" (gcrypt base64) ,guile-gcrypt)))
  (skip ())
  (files (libraries
           ((directory
              "neocities"
              ((scheme-file "api")
               (scheme-file "requests")
               (scheme-file "cli")
               (scheme-file "mime")))))
         (tests
           ((directory "tests" ((scheme-file "requests")))))
         (programs
           ((directory "scripts" ((in-file "neocities")))))
         (documentation
           ((text-file "ChangeLog")
            (text-file "AUTHORS")
            (text-file "NEWS")
            (markdown-file "README")
            (text-file "HACKING")
            (text-file "COPYING")
            (directory ".github" ((unknown-type "FUNDING.yml")))
            (directory "doc" ((texi-file "neocities")))))
         (infrastructure
           ((scheme-file "guix")
            (scheme-file "hall")))))
