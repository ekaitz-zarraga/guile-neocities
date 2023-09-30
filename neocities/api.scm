;; neocities/api --- neocities api implementation -*- coding: utf-8 -*-
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

(define-module (neocities api)
  #:use-module (neocities requests)
  #:use-module (rnrs base)
  #:use-module (srfi srfi-9)
  #:export (
    make-neocities-api
    make-neocities-auth-basic
    make-neocities-auth-api-key
    neocities-delete
    neocities-info
    neocities-list
    neocities-key
    neocities-upload
    neocities-success?
))

(define-record-type <neocities-auth-api-key>
  (make-neocities-auth-api-key key)
  neocities-auth-api-key?
  (key neocities-auth-api-key))

(define-record-type <neocities-auth-basic>
  (make-neocities-auth-basic username password)
  neocities-auth-basic?
  (username neocities-auth-basic-username)
  (password neocities-auth-basic-password))

(define (encode-auth auth)
  (cond
    ((neocities-auth-api-key? auth)
     (encode-bearer-auth-header (neocities-auth-api-key auth)))
    ((neocities-auth-basic? auth)
     (encode-basic-auth-header
       (neocities-auth-basic-username auth)
       (neocities-auth-basic-password auth)))
    (else
     (throw 'neocities "Authentication scheme not supported"))))

(define-record-type <neocities-api>
  (_make-neocities-api hostname port auth)
  neocities-api?
  (hostname neocities-api-hostname)
  (port neocities-api-port)
  (auth neocities-api-auth neocities-api-auth-set!))

(define* (make-neocities-api hostname auth #:optional port)
  (_make-neocities-api hostname port auth))


(define* (neocities-delete api files)
  (when (not (list? files))
    (throw 'neocities "files to delete must be a list"))

  (let ((url (neocities-url "delete"
                            #:port (neocities-api-port api)
                            #:hostname (neocities-api-hostname api)
                            #:querystring `(("filenames[]" . ,files)))))
    (neocities-request
      'POST
      url
      #:auth (encode-auth (neocities-api-auth api)))))


(define* (neocities-list api #:optional path)
  (let ((url (neocities-url "list"
                            #:hostname (neocities-api-hostname api)
                            #:querystring (if path `(("path" ,path)) '()))))
    (neocities-request
      'GET
      url
      #:auth (encode-auth (neocities-api-auth api)))))

(define* (neocities-info api #:optional sitename)
  ;; It can be unauthenticated, but this is only the authenticated version
  (let ((url (neocities-url "info"
                            #:hostname (neocities-api-hostname api)
                            #:querystring (if sitename
                                            `(("sitename" ,sitename))
                                            '()))))
    (neocities-request
      'GET
      url
      #:auth (encode-auth (neocities-api-auth api)))))

(define* (neocities-key api)
  (let ((url (neocities-url "key"
                            #:hostname (neocities-api-hostname api))))
    (neocities-request
      'GET
      url
      #:auth (encode-auth (neocities-api-auth api)))))

(define* (neocities-upload api files)
  "files is an alist with the filename and destination"
  (let ((url (neocities-url "upload"
                            #:hostname (neocities-api-hostname api))))
    (let-values (((boundary body) (encode-multipart-body files)))
      (neocities-request
        'POST
        url
        #:content-type (string-append "multipart/form-data; boundary="  boundary)
        #:body body
        #:auth (encode-auth (neocities-api-auth api))))))

(define (neocities-success? response)
  (string=? (assoc-ref response "result") "success"))
