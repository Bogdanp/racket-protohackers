#lang racket/base

(require racket/tcp)

(provide
 run-server
 run-server*)

(define (run-server host port handle
                    #:backlog [backlog 511]
                    #:reuse? [reuse? #t]
                    #:timeout-evt-proc [make-timeout-evt values]
                    #:listen-proc [listen tcp-listen]
                    #:accept-proc [accept tcp-accept/enable-break]
                    #:close-proc [close tcp-close])
  (define listener
    (listen port backlog reuse? host))
  (define server-custodian
    (make-custodian))
  (define server-thd
    (parameterize ([current-custodian server-custodian])
      (thread
       (lambda ()
         (with-handlers ([exn:break? void])
           (let loop ()
             (parameterize-break #f
               (define-values (in out)
                 (accept listener))
               (define client-custodian
                 (make-custodian))
               (define client-thd
                 (thread
                  (lambda ()
                    (break-enabled #t)
                    (parameterize ([current-custodian client-custodian])
                      (handle in out)))))
               (thread
                (lambda ()
                  (sync (make-timeout-evt client-thd))
                  (close-output-port out)
                  (close-input-port in)
                  (custodian-shutdown-all client-custodian))))
             (loop)))
         (close listener)))))
  (lambda ()
    (break-thread server-thd)
    (thread-wait server-thd)
    (custodian-shutdown-all server-custodian)))

(define run-server*
  (make-keyword-procedure
   (lambda (kws kw-args . args)
     (define stop (keyword-apply run-server kws kw-args args))
     (with-handlers ([exn:break? void])
       (sync never-evt))
     (stop))))
