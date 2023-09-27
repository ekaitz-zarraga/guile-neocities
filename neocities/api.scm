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
  (make-neocities-api hostname auth)
  neocities-api?
  (hostname neocities-api-hostname)
  (auth neocities-api-auth neocities-api-auth-set!))



(define* (neocities-delete api files)
  (when (not (list? files))
    (throw 'neocities "files to delete must be a list"))

  (let ((url (neocities-url "delete"
                            #:hostname (neocities-api-hostname api)
                            #:querystring `(("files" . ,files)))))
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
