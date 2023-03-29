#lang racket/base

(require racket/tcp)

(define (handle in out)
  (define-values (proxied-in proxied-out)
    (tcp-connect "chat.protohackers.com" 16963))
  (thread
   (lambda ()
     (pump proxied-in out)))
  (pump in proxied-out))

(define (pump in out)
  (define buf (make-bytes 4096))
  (let loop ([data #""])
    (define len
      (read-bytes-avail! buf in))
    (cond
      [(eof-object? len)
       (write-bytes (rewrite data) out)
       (flush-output out)]
      [else
       (loop (drain (bytes-append data (subbytes buf 0 len)) out))])))

(define (drain data out)
  (let loop ([data data])
    (cond
      [(bytes-index-of data 10)
       => (λ (idx)
            (write-bytes (rewrite (subbytes data 0 idx)) out)
            (write-byte 10 out)
            (flush-output out)
            (loop (subbytes data (add1 idx))))]
      [else data])))

(define (rewrite data)
  (regexp-replace*
   #px#"(.?)(7[a-zA-Z0-9]{25,34})(.?)" data
   (λ (bs pre _addr post)
     (cond
       [(and (member pre '(#"" #" "))
             (member post '(#"" #" ")))
        (bytes-append pre #"7YWHMfk9JZe0LM0g1ZauHuiSxhI" post)]
       [else bs]))))

(define (bytes-index-of bs b)
  (for/first ([o (in-bytes bs)]
              [i (in-naturals)]
              #:when (= o b))
    i))

(module+ main
  (require "common.rkt")
  (run-server* "0.0.0.0" 8111 handle))
