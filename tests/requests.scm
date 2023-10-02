;; tests/requests --- neocities requests tester -*- coding: utf-8 -*-
;;
;; Copyright (C) 2023 Ekaitz Zarraga <ekaitz@elenq.tech>
;;
;; Author: Ekaitz Zarraga <ekaitz@elenq.tech>
;;
;; This file is part of guile-neocities.
;;
;; guile-neocities is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3 of the License, or (at your option)
;; any later version.
;;
;; guile-neocities is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with guile-neocities; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;; Boston, MA  02111-1307,  USA       gnu@gnu.org

(define-module (tests requests)
  #:use-module (neocities requests)
  #:use-module (scheme base)
  #:use-module (web uri)
  #:use-module (web server)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-64))


(test-begin "URL")

  (test-group "Creates url"
              (define url (neocities-url "info" #:hostname "neocities.org"))
              (test-assert (uri? url)))


  (test-group "Creates url with port and host"
              (define url (neocities-url "info" #:hostname "neocities.org" #:port 80))
              (test-assert (= (uri-port url) 80))
              (test-assert (string=? (uri-host url) "neocities.org")))


  (test-group "Encodes simple querystring"
              (define url (neocities-url
                            "info" #:hostname "neocities.org" #:port 80
                            #:querystring '(("file" . "COSA CON ESPACIOS"))))
              (test-assert (string=? (uri-query url) "file=COSA%20CON%20ESPACIOS")))


  (test-group "Encodes array in querystring"
              (define url (neocities-url
                            "info" #:hostname "neocities.org" #:port 80
                            #:querystring '(("file[]" . ("COSA CON ESPACIOS" "otra cosa")))))
              (test-assert (string=?
                             (uri-query url)
                             "file%5B%5D=COSA%20CON%20ESPACIOS&file%5B%5D=otra%20cosa")))
(test-end "URL")



(test-begin "Request")

(define test-port 8080)

(define* (serve-one-client-in-thread handler)
         (let* ((impl      (lookup-server-impl 'http))
                (server    (open-server impl `(#:port ,test-port))))
           (begin-thread (serve-one-client handler impl server '())
                         (close-server impl server))))

(test-group "Request arrived with empty body"
  (let* ((request-ret #f)
         (request-body-ret #f)
         (handler (lambda (request body)
                    (set! request-ret request)
                    (set! request-body-ret body)
                    (values '((content-type . (text/plain)))
                            "\"Hello World!\"")))
        (th (serve-one-client-in-thread handler)))
    (neocities-request
      'GET
      (neocities-url "info"
                     #:hostname "localhost"
                     #:port test-port
                     #:insecure #t))
    (join-thread th)
    (test-assert (and request-ret (not request-body-ret)))))


(test-end "Request")
