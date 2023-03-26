#lang racket/base

(require racket/match
         racket/math
         racket/port
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
  (require "common.rkt")
  (run-server* "0.0.0.0" 8111 handle))
