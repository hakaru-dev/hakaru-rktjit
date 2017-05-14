#lang racket

(require racket/splicing)

(require "ast.rkt")

(provide gensym^
         symbol-append
         map-ast
         real2prob
         prob2real
         logsumexp2
         logspace-add
         one-of-type
         zero-of-type)

(splicing-let ([gensym-hash (make-hash)])
  (define (gensym^ sym [sep ""])
    (define n (add1 (hash-ref gensym-hash sym 0)))
    (hash-set! gensym-hash sym n)
    (string->symbol (string-append (symbol->string sym)
                                   sep
                                   (number->string n)))))
(define (symbol-append s1 s2)
  (string->symbol (string-append (symbol->string s1)
				 (symbol->string s2))))

;; func: ast -> ast, should return the same ast if no change
;; this map will call the func on each ast node
;; and then recursively call it again in on the output from the func
(define (map-ast func ast)
  (define (m body)
    (match (func body)
      [(expr-fun args ret-type body)
       (expr-fun args ret-type (m body))]
      [(expr-if t tst thn els)
       (expr-if t (m tst) (m thn) (m els))]
      [(expr-app t rt rds)
       (expr-app t (m rt) (map m rds))]
      [(expr-let type var val b)
       (expr-let type var (m val) (m b))]
      [(expr-sum t i start end b)
       (expr-sum t i (m start) (m end) (m b))]
      [(expr-prd t i start end b)
       (expr-prd t i (m start) (m end) (m b))]
      [(expr-arr t i end b)
       (expr-arr t i (m end) (m b))]
      [(expr-val t v)
       (expr-val t v)]
      [(expr-intr s)
       (expr-intr s)]
      [(expr-var t s o)
       (expr-var t s o)]))
  (m ast))


(require math/flonum)
(define (prob2real x) (+ (flexpm1 x) 1))
(define (real2prob x) (fllog1p (- x 1)))
(define (logsumexp2 a b)
  (if (> a b)
      (+ a (fllog1p (+ (flexpm1 (- b a)) 1)))
      (+ b (fllog1p (+ (flexpm1 (- a b)) 1)))))

(define (one-of-type t)
  (if (equal? t 'prob)
      (real2prob 1.0)
      1.0))
(define (zero-of-type t)
  (if (equal? t 'prob)
      (real2prob 0.0)
      0.0))

(define logspace-add (λ args (real2prob (apply + (map prob2real args)))))
