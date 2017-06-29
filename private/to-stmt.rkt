#lang racket

(define to-stmt
  (expr/pass
   [(expr-let* tls vars vals body)
    (stmt-block
     `(,@(for/list ([t tls]
                    [var vars]
                    [val vals])
           (stmt-set! var t val))
       ,body))]
   [(expr-let tl var val body)
    (stmt-block
     (stmt-set! var tl val)
     body)]))
