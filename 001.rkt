#lang racket/base

(require json
         racket/match)

(define (prime? n)
  (let ([n (truncate n)])
    (and (not (negative? n))
         (not (= n 0))
         (not (= n 1))
         (or (= n 2)
             (not
              (for/or ([i (in-range 2 (add1 (sqrt n)))])
                (zero? (modulo n i))))))))

(define (handle in out)
  (with-handlers ([exn:fail?
                   (λ (e)
                     ((error-display-handler) (format "client error: ~a" (exn-message e)) e)
                     (displayln "request malformed" out))])
    (let loop ()
      (define line (read-line in))
      (match (string->jsexpr line)
        [(hash-table
          ['method "isPrime"]
          ['number (? number? n)])
         (write-json (hasheq 'method "isPrime" 'prime (prime? n)) out)
         (newline out)
         (flush-output out)
         (loop)]
        [_
         (displayln "request malformed" out)]))))

(module+ main
  (require "common.rkt")
  (run-server* "0.0.0.0" 8111 handle))
