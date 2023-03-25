#lang racket/base

(require racket/match
         racket/math
         racket/port
         racket/tcp
         "002.bnf")

(define (get-avg-price prices min-time max-time)
  (for/fold ([n 0] [s 0] #:result (if (zero? n) 0 (exact-round (/ s n))))
            ([(timestamp price) (in-hash prices)]
             #:when (and (>= timestamp min-time)
                         (<= timestamp max-time)))
    (values (add1 n) (+ s price))))

(define (handle in out)
  (let loop ([prices (hasheqv)])
    (define data (read-bytes 9 in))
    (unless (eof-object? data)
      (match (call-with-input-bytes data Message)
        [`((char_1 . #\I) (Timestamp_1 . ,timestamp) (Price_1 . ,price))
         (loop (hash-set prices timestamp price))]

        [`((char_1 . #\Q) (MinTime_1 . ,min-time) (MaxTime_1 . ,max-time))
         (un-Price (get-avg-price prices min-time max-time) out)
         (flush-output out)
         (loop prices)]))))

(module+ main
  (define listener
    (tcp-listen 8111 512 #t "0.0.0.0"))
  (define server-custodian
    (make-custodian))
  (with-handlers ([exn:break? void])
    (parameterize ([current-custodian server-custodian])
      (let loop ()
        (define-values (in out)
          (tcp-accept listener))
        (define client-thd
          (thread
           (lambda ()
             (handle in out))))
        (thread
         (lambda ()
           (sync client-thd)
           (close-output-port out)
           (close-input-port in)))
        (loop))))
  (custodian-shutdown-all server-custodian)
  (tcp-close listener))
