(define-module (neocities cli)
  #:use-module (neocities api)
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
  (let ((result (neocities-list %api (and (not (null? args)) (car args)))))
    (if (neocities-success? result)
      (display (assoc-ref result 'files)) ;; TODO Display in table
      (format (current-error-port) "Not successful"))))

(define (neocities-cmd-key args)
  (when (not (= 0 (length args)))
    (format (current-error-port) "USAGE: neocities key~&")
    (exit 1))
  (let ((result (neocities-key %api)))
    (if (neocities-success? result)
      (format #t "~a~&" (assoc-ref result "api_key"))
      (format (current-error-port) "Not successful"))))

(define (neocities-cmd-info args)
  (when (not (= 0 (length args)))
    (format (current-error-port) "USAGE: neocities info~&")
    (exit 1))
  (let ((result (neocities-info %api)))
    (if (neocities-success? result)
      (display (assoc-ref result "info")) ;; TODO display in table
      (format (current-error-port) "Not successful"))))

(define (neocities-cmd-delete args) #f)
(define (neocities-cmd-upload args) #f)

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
