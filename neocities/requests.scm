;; neocities/requests --- neocities http request impl -*- coding: utf-8 -*-
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

(define-module (neocities requests)
  #:use-module (neocities mime)
  #:use-module (ice-9 match)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 binary-ports)
  #:use-module (json)
  #:use-module (gcrypt base64)
  #:use-module (gcrypt random)
  #:use-module (rnrs base)
  #:use-module (scheme base)
  #:use-module (web uri)
  #:use-module (web client)
  #:use-module (web response)
  #:export (neocities-url
            neocities-request
            encode-basic-auth-header
            encode-bearer-auth-header
            encode-multipart-body))

(define* (neocities-url endpoint #:key (querystring '())
                                       (hostname     #f)
                                       (insecure     #f)
                                       (port         #f))
  (define (encode-querystring querystring)
    (string-join
      (map (lambda (x)
             (let ((key   (car x))
                   (value (cdr x)))
               (if (= 1 (length value))
                 (string-append (uri-encode key) "=" (uri-encode (car value)))
                 (string-join
                   (map (lambda (y)
                          (string-append (uri-encode key) "=" (uri-encode y)))
                        value)
                   "&"))))
           querystring)
      "&"))

  (build-uri (if insecure 'http 'https)
           #:host (or hostname "neocities.org")
           #:port port
           #:path (string-append "/api/" endpoint)
           #:query (encode-querystring querystring)))

(define (encode-multipart-body files)
  "files is an alist with the filename and destination"

  (define (name file)
    (car file))

  (define (destination file)
    (cdr file))

  (define (type file)
    (let* ((extension (car (last-pair (string-split (name file) #\.))))
           (type (assoc-ref mime-types extension)))
      (if (string? type)
          type
          (throw 'neocities "Unknown mime-type"))))

  (define (encode-file file)
    (bytevector-append
      (string->utf8
        (string-append
          "Content-Disposition: form-data;"
          "filename=\"" (destination file) "\"\r\n"
          "Content-Type: " (type file) " \r\n\r\n"))
      (call-with-input-file (name file) get-bytevector-all #:binary #t)
      (string->utf8 "\r\n")))

  (define boundary
    (string-append "----------------------------------------------------------"
      (random-token)))

  (values boundary
    (apply bytevector-append
           (map (lambda (file i)
                  (bytevector-append
                    (string->utf8 (string-append boundary "\r\n"))
                    (encode-file file)
                    (if (= (+ i 1) (length files))
                      (string->utf8 (string-append "\r\n" boundary "--\r\n\r\n"))
                      #vu8())))
                files
                (iota (length files))))))

(define (encode-basic-auth-header username password)
  (string-append "Basic "
                 (base64-encode
                   (string->utf8 (string-append username ":" password)))))

(define (encode-bearer-auth-header apikey)
  (string-append "Bearer " apikey))


(define* (neocities-request method
                            url
                            #:key (body  #f)
                                  (auth  #f)
                                  (content-type #f))


  (define (decode-body response body)
    (match (response-content-type response)
           (('text/plain ('charset . "utf-8"))
            (utf8->string body))
           (('text/html ('charset . "utf-8"))
            (utf8->string body))
           (('application/json . rest)
            (json-string->scm (utf8->string body)))
           (else body))) ;; Don't know how to decode, leave it

  (let-values (((response body)
                (http-request
                  url
                  #:method method
                  #:body body
                  #:version '(1 . 1)
                  #:streaming? #f
                  #:headers (filter identity
                              (list
                                (and content-type `(Content-Type . ,content-type))
                                (and auth         `(Authorization . ,auth))))
                  #:decode-body? #f)))
    (values response (decode-body response body))))
