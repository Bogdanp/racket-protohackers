#lang racket/base

(require racket/match
         racket/tcp)

(define (handle in out)
  (let loop ()
    (match (read-bytes 4096 in)
      [(? eof-object?)
       (void)]
      [bs
       (write-bytes bs out)
       (loop)])))

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
