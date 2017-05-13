#lang racket

(provide (all-defined-out))

(define (print-expr e)
  (define pe print-expr)
  (match e
    [(expr-fun args ret-type body)
     `(function ,(map pe args) ,(pe body))]
    [(expr-var type sym orig)
     orig]
    [(expr-arr type index size body)
     `(array ,(pe index) ,(pe size) ,(pe body))]
    [(expr-sum type index start end body)
     `(summate (,(pe index) ,(pe start) ,(pe end)) ,(pe body))]
    [(expr-prd type index start end body)
     `(product (,(pe index) ,(pe start) ,(pe end)) ,(pe body))]
    [(expr-if type tst thn els)
     `(if ,(pe tst) ,(pe thn) ,(pe els))]
    [(expr-app type rator rands)
     `(,(pe rator) ,@(map pe rands))]
    [(expr-let type var val body)
     `(let (,(pe var) ,(pe val)) ,(pe body))]
    [(expr-intr s)
     s]
    [(expr-val t v)
     v]
    [else e]))

(struct expr-fun  (args ret-type body)
  #:methods gen:custom-write
  [(define write-proc
     (lambda (fn port mode) (fprintf port "~a" (print-expr fn))))])
(struct expr-let  (type var val body))
(struct expr-var  (type sym orig)
  #:methods gen:custom-write
  [(define write-proc
     (lambda (v port mode) (fprintf port "~a" (expr-var-sym v))))])
(struct expr-arr  (type index size body))
(struct expr-sum  (type index start end body))
(struct expr-prd  (type index start end body))
(struct expr-if   (type tst thn els))
(struct expr-app  (type rator rands))
(struct expr-val  (type v))
(struct expr-intr (sym))

(define internal-ops
  (apply set '(size index recip nat2prob prob2real + * == and < or not)))
(define sum-prod-loops (set 'summate 'product))
(define internal-loop-ops
  (set 'summate 'product 'array))
