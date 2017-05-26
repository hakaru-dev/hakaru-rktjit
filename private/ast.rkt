#lang racket

(provide (all-defined-out))

(define (print-expr e)
  (define pe print-expr)
  (match e
    [(expr-mod main fns)
     `((main ,(pe main))
       ,@(for/list [(fn fns)]
           `(,(car fn) ,(pe (cdr fn)))))]
    [(expr-fun args ret-type body)
     `(function ,(map pe args) ,(pe body))]
    [(expr-var type sym orig) orig]
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
    [(expr-intr s) s]
    [(expr-intrf s) s]
    [(expr-val t v) v]
    [else `(unknown ,e)]))

(struct expr-mod  (main fns))
(struct expr-fun  (args ret-type body))
(struct expr-let  (type var val body))
(struct expr-var  (type sym orig))
(struct expr-arr  (type index size body))
(struct expr-sum  (type index start end body))
(struct expr-prd  (type index start end body))
(struct expr-if   (type tst thn els))
(struct expr-app  (type rator rands))
(struct expr-val  (type v))
(struct expr-intr (sym))
(struct expr-intrf (sym))
(define internal-ops
  (apply set '(size index recip nat2prob prob2real + * == and < or not)))
(define sum-prod-loops (set 'summate 'product))
(define internal-loop-ops
  (set 'summate 'product 'array))

(define (typeof ast)
  (match ast
    [(expr-fun args ret-type body)
     'fn]
    [(expr-if t tst thn els)
     t]
    [(expr-app t rt rds)
     t]
    [(expr-let t var val b)
     t]
    [(expr-sum t i start end b)
     t]
    [(expr-prd t i start end b)
     t]
    [(expr-arr t i end b)
     t]
    [(expr-val t v)
     t]
    [(expr-intr s)
     '*]
    [(expr-intrf s)
     '!]
    [(expr-var t s o)
     t]))
