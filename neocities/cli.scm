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


(define (neocities-cmd-list args)
  (when (not (= 1 (length args)))
    (format (current-error-port) "USAGE: neocities list DIRECTORY~&")
    (exit 1))
  (neocities-list %api (car args)))

(define (neocities-cmd-key args)
  (when (not (= 0 (length args)))
    (format (current-error-port) "USAGE: neocities key~&")
    (exit 1))
  (let ((key-pair (assoc "api_key" (neocities-key %api))))
    (format #t "~a~&" (cdr key-pair))))

(define (neocities-cmd-upload args) #f)
(define (neocities-cmd-delete args) #f)
(define (neocities-cmd-info args) #f)

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

(define (lookup-command command)
  (let ((value (assoc command neocities-commands)))
    (and value (cdr value))))

(define (neocities-run command args)
  ((or (lookup-command command) (command-not-known command)) args))
