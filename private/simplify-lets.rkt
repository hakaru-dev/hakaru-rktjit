#lang racket
(require "ast.rkt")
(require "pass-utils.rkt")
(provide simplify-lets)
(define (sl e env)
  (fill-expr-pass
   (curryr sl env) e
   [(expr-let t v val body)
    (if (expr-var? val)
        (begin
          (printf "replacing: ~a with: ~a\n" (print-expr v) (print-expr val))
          (sl body (hash-set env v val)))
        (expr-let t v (sl val env) (sl body env)))]
   [v #:when (hash-has-key? env v)
      (printf "replaced from env: ~a with: ~a\n" (print-expr v) (print-expr (hash-ref env v)))
      (hash-ref env v)]))

(define (simplify-lets e)
  (sl e (make-immutable-hash)))
