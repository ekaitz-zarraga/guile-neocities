(define-module (neocities requests)
  #:use-module (neocities mime)
  #:use-module (json)
  #:use-module (ice-9 match)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 binary-ports)
  #:use-module (gcrypt base64)
  #:use-module (gcrypt random)
  #:use-module (rnrs base)
  #:use-module (rnrs bytevectors)
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
                                       (hostname     #f))
  (define (encode-querystring querystring)
    (string-join
      (map (lambda (x)
             (let ((key   (car x))
                   (value (cdr x)))
               (if (list? value)
                 (string-join
                   (map (lambda (y)
                          (string-append (uri-encode key) "[]=" (uri-encode y)))
                        value)
                   "&")
                 (string-append (uri-encode key) "=" (uri-encode value)))))
           querystring)
      "&"))

  ;; This is for testing locally :)
  #;(build-uri 'http #:host "localhost"
                      #:port 1234
                      #:path (string-append "/api/" endpoint)
                      #:query (encode-querystring querystring))
  (build-uri 'https
           #:host (or hostname "neocities.org")
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

  (define (response-error res)
    `(("response-code" . ,(response-code res))
      ("response-phrase" .
       ,(response-reason-phrase res))
      ("response" .
       ,(if (bytevector? body)
          (utf8->string body)
          body))))

  (define (response-ok? response)
    (> 100 (- (response-code response) 200) -1))

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
                  #:decode-body? #t)))
    (if (response-ok? response)
        (json-string->scm (utf8->string body))
        (throw 'neocities (response-error response)))))
