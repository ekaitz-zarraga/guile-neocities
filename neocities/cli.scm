(define-module (neocities cli)
  #:use-module (neocities api)
  #:use-module (srfi srfi-11)
  #:use-module (ice-9 format)
  #:export (
    neocities-run
))

(define %user (getenv "NEOCITIES_USER"))
(define %pass (getenv "NEOCITIES_PASS"))
(define %key  (getenv "NEOCITIES_KEY"))
(define %host (or (getenv "NEOCITIES_HOST") "neocities.org"))

(define %auth
  (cond
    (%key (make-neocities-auth-api-key %key))
    ((and %user %pass) (make-neocities-auth-basic %user %pass))
    (else (begin
            (format (current-error-port) "ERROR: Missing authentication method~&")
            (exit 1)))))

(define %api (make-neocities-api %host %auth))


;; TODO MAKE MACRO: if-neocities-success?
;(define-syntax (syntax-rules))

(define (neocities-cmd-list args)
  (when (< 1 (length args))
    (format (current-error-port) "USAGE: neocities list [DIRECTORY]~&")
    (exit 1))
  (let-values (((response body) (neocities-list %api (and (not (null? args)) (car args)))))
    (if (neocities-success? body)
        (format #t
                "Updated at~32tDir~36tFilename~86tSize~91tSHA-1~&~:{~A~33t~@[d~]~36t~A~84t~@[~6d~]~91t~@[~A~]~&~}"
                (map
                  (lambda (file) (list
                                   (assoc-ref file "updated_at")
                                   (assoc-ref file "is_directory")
                                   (assoc-ref file "path")
                                   (assoc-ref file "size")
                                   (assoc-ref file "sha1_hash")))
                  (vector->list (assoc-ref body "files"))))
      (format (current-error-port) "~A~&" (assoc-ref body "message")))))

(define (neocities-cmd-key args)
  (when (not (= 0 (length args)))
    (format (current-error-port) "USAGE: neocities key~&")
    (exit 1))
  (let-values (((response body) (neocities-key %api)))
    (if (neocities-success? body)
      (format #t "~A~&" (assoc-ref body "api_key"))
      (format (current-error-port) "~A~&" (assoc-ref body "message")))))

(define (neocities-cmd-info args)
  (when (not (= 0 (length args)))
    (format (current-error-port) "USAGE: neocities info~&")
    (exit 1))
  (let-values (((response body) (neocities-info %api)))
    (if (neocities-success? body)
      (format #t "~:{~a: ~a~&~}"
        (map (lambda (el) (list (car el) (cdr el)))
             (let ((info (assoc-ref body "info")))
               (assoc-set! info "tags"
                 (string-join (vector->list (assoc-ref info "tags")) ", ")))))
      (format (current-error-port) "~A~&" (assoc-ref body "message")))))

(define (neocities-cmd-delete args)
  (when (= 0 (length args))
    (format (current-error-port) "USAGE: neocities delete FILE [...]~&")
    (exit 1))
  (let-values (((response body) (neocities-delete %api args)))
    (if (neocities-success? body)
      (format #t "~A~&" (assoc-ref body "message"))
      (format (current-error-port) "~A~&" (assoc-ref body "message")))))

(define (neocities-cmd-upload args)
  (define (pair-args args)
    (map (lambda (i)
           (cons (list-ref args (* 2 i)) (list-ref args (+ 1 (* 2 i)))))
         (iota (/ (length args) 2))))

  (when (or (> 1 (length args)) (not (= 0 (modulo (length args) 2))))
    (format (current-error-port) "USAGE: neocities upload LOCAL_FILE DESTINATION [...]~&")
    (exit 1))
  (let-values (((response body) (neocities-upload %api (pair-args args))))
    (if (neocities-success? body)
      (format #t "~A~&" (assoc-ref body "message"))
      (format (current-error-port) "~A~&" (assoc-ref body "message")))))

(define (command-not-known command)
  (format (current-error-port) "Command not known ~A~&" command)
  (exit 1))

;; TODO: generate docs automatically from here?
(define neocities-commands
  `((list   . ,neocities-cmd-list)
    (upload . ,neocities-cmd-upload)
    (delete . ,neocities-cmd-delete)
    (info   . ,neocities-cmd-info)
    (key    . ,neocities-cmd-key)))

(define (neocities-run command args)
  ((or (assoc-ref neocities-commands command)
       (command-not-known command))
   args))
