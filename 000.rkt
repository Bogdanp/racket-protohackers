#lang racket/base

(require racket/match)

(define (handle in out)
  (let loop ()
    (match (read-bytes 4096 in)
      [(? eof-object?)
       (void)]
      [bs
       (write-bytes bs out)
       (loop)])))

(module+ main
  (require "common.rkt")
  (run-server* "0.0.0.0" 8111 handle))
