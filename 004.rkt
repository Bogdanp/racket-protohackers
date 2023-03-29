#lang racket/base

(require racket/match
         racket/port)

(define db
  (make-hash `(("version" . "Ken's Key-Value Store 1.0"))))

(define (handle in out)
  (match (port->string in)
    [(regexp #rx"^([^=]*)=(.*)$" (list _ key value))
     (unless (equal? key "version")
       (hash-set! db key value))]
    [key
     (define value (hash-ref db key #f))
     (display key out)
     (when value
       (display "=" out)
       (display value out))]))

(module+ main
  (require racket/udp
           "common.rkt")

  (define (udp-listen port _backlog reuse? host)
    (define socket (udp-open-socket))
    (begin0 socket
      (udp-bind! socket host port reuse?)))

  (define (udp-accept listener)
    (define buf (make-bytes 65536))
    (parameterize-break #f
      (define-values (len hostname port)
        (udp-receive!/enable-break listener buf))
      (define client-in (open-input-bytes (subbytes buf 0 len)))
      (define-values (pipe-in client-out)
        (make-pipe))
      (thread
       (lambda ()
         (let loop ()
           (define len (read-bytes! buf pipe-in))
           (unless (eof-object? len)
             (udp-send-to listener hostname port buf 0 len)
             (loop)))))
      (values client-in client-out)))

  (run-server* "0.0.0.0" 8111 handle
               #:listen-proc udp-listen
               #:accept-proc udp-accept
               #:close-proc udp-close))
