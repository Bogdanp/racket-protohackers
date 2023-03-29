#lang racket/base

(require binfmt/runtime/parser
         binfmt/runtime/res
         binfmt/runtime/unparser)

(provide
 (rename-out
  [parse-Str Str]
  [unparse-Str un-Str]))

(define (parse-Str in)
  (res-bind
   (parse-u8 in)
   (lambda (len)
     (define bs
       (read-bytes len in))
     (cond
       [(eof-object? bs)
        (err "unexpected EOF while reading string")]
       [(< (bytes-length bs) len)
        (err "unexpected EOF while reading string")]
       [else
        (ok bs)]))))

(define (unparse-Str out bs)
  (res-bind
   (unparse-u8 out (bytes-length bs))
   (lambda (_)
     (begin0 (ok bs)
       (write-bytes bs out)))))
