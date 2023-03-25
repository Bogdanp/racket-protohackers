#lang racket/base

(require racket/match
         racket/string
         racket/tcp)

(define room-ch (make-channel))
(define room-thd
  (thread/suspend-to-kill
   (lambda ()
     (let loop ([users (hasheq)]
                [reqs null])
       (apply
        sync
        (handle-evt
         room-ch
         (lambda (msg)
           (match msg
             [`(join ,name ,out ,res-ch ,nack)
              (cond
                [(hash-has-key? users name)
                 (define req `((fail "username-taken") ,res-ch ,nack))
                 (loop users (cons req reqs))]
                [else
                 (define req `((ok ,(hash-keys users)) ,res-ch ,nack))
                 (broadcast users (format "* ~a has joined the room~n" name))
                 (loop (hash-set users name out) (cons req reqs))])]
             [`(broadcast ,name ,message ,res-ch ,nack)
              (define req `((ok) ,res-ch ,nack))
              (broadcast (hash-remove users name) (format "[~a] ~a~n" name message))
              (loop users (cons req reqs))]
             [`(leave ,name ,res-ch ,nack)
              (define req `((ok) ,res-ch ,nack))
              (define remaining-users (hash-remove users name))
              (broadcast remaining-users (format "* ~a has left the room~n" name))
              (loop remaining-users (cons req reqs))]
             [_
              (log-warning "malformed message: ~s" msg)
              (loop users reqs)])))
        (append
         (for/list ([req (in-list reqs)])
           (match-define `(,res ,res-ch ,_) req)
           (handle-evt
            (channel-put-evt res-ch res)
            (λ (_) (loop users (remq req reqs)))))
         (for/list ([req (in-list reqs)])
           (match-define `(,_ ,_ ,nack) req)
           (handle-evt nack (λ (_) (loop users (remq req reqs)))))))))))

(define (make-room-evt command . args)
  (define res-ch (make-channel))
  (nack-guard-evt
   (lambda (nack)
     (begin0 res-ch
       (thread-resume room-thd (current-thread))
       (channel-put room-ch `(,command ,@args ,res-ch ,nack))))))

(define (handle in out)
  (fprintf* out "Welcome to budgetchat! What shall I call you?~n")
  (match (read-line in 'any)
    [(regexp #px"^[a-zA-Z0-9]{1,16}$" (list (app string->symbol name)))
     (match (sync (make-room-evt 'join name out))
       [`(fail ,message)
        (fprintf out "error: ~a~n" message)]
       [`(ok ,names)
        (fprintf* out "* The room contains: ~a~n" (string-join (map symbol->string (sort names symbol<?))))
        (with-handlers ([exn:fail? (λ (e) ((error-display-handler) (format "client error: ~a" (exn-message e)) e))])
          (let loop ()
            (define data
              (read-line in 'any))
            (unless (eof-object? data)
              (sync (make-room-evt 'broadcast name data))
              (loop))))
        (sync (make-room-evt 'leave name))])]
    [_
     (fprintf* out "error: invalid name~n")]))

(define (broadcast users message)
  (for ([out (in-hash-values users)])
    (with-handlers ([exn:fail? void])
      (fprintf* out message))))

(define (fprintf* out msg . args)
  (apply fprintf out msg args)
  (flush-output out))

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
