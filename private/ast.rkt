#lang racket

(provide (all-defined-out))


(define (print-expr e)
  (match e
    [(expr-mod main fns)
     `((main ,(pe main))
       ,@(for/list [(fn fns)]
           `(,(car fn) ,(pe (cdr fn)))))]
    [(expr-fun args ret-type body)
     `(function ,(map pe args) ,(pe body))]
    [(expr-var type sym orig) sym]
    [(expr-arr type index size body)
     `(array ,(pe index) ,(pe size) ,(pe body))]
    [(expr-sum type index start end body)
     `(summate (,(pe index) ,(pe start) ,(pe end)) ,(pe body))]
    [(expr-prd type index start end body)
     `(product (,(pe index) ,(pe start) ,(pe end)) ,(pe body))]
    [(expr-bucket type start end reducer)
     `(bucket ,(pe start) ,(pe end) ,(pr reducer))]
    [(expr-bind var body)
     `(/ ,(pe var) -> ,(pe body))]
    [(expr-match type tst branches)
     `(match ,(pe tst) ,@(map pe branches))]
    [(expr-branch pat body)
     `[,(pp pat) ,(pe body)]]
    [(expr-if type tst thn els)
     `(if ,(pe tst) ,(pe thn) ,(pe els))]
    [(expr-app type rator rands)
     `(,(pe rator) ,@(map pe rands))]
    [(expr-let type var val body)
     `(let (,(pe var) ,(pe val)) ,(pe body))]
    [(expr-intr s) s]
    [(expr-intrf s) s]
    [(expr-val t v) v]
    [else `(? ,e)]))
(define pe print-expr)
(define (pr red)
  (match red
    [(reducer-split e a b) `(split ,(pe e) ,(pr a) ,(pr b))]
    [(reducer-fanout a b) `(fanout ,(pr a) ,(pr b))]
    [(reducer-add i) `(add ,(pe i))]
    [(reducer-nop) `(nop)]
    [(reducer-index i e b) `(index ,(pe i) ,(pe e) ,(pr b))]))
(define (pp pat)
  (match pat
    [(pat-var) 'var]
    [(pat-true) 'true]
    [(pat-false) 'false]
    [(pat-pair p) `(pair ,(map pp p))]
    [(pat-ident) 'rec]))

(struct expr-mod  (main fns) #:prefab)
(struct expr-fun  (args ret-type body) #:prefab)
(struct expr-let  (type var val body) #:prefab)
(struct expr-var  (type sym orig) #:prefab)
(struct expr-arr  (type index size body) #:prefab)
(struct expr-sum  (type index start end body) #:prefab)
(struct expr-prd  (type index start end body) #:prefab)
(struct expr-bucket (type start end reducer) #:prefab)
(struct expr-branch (pat body) #:prefab)
(struct expr-match  (type tst branches) #:prefab)
(struct expr-bind (var body) #:prefab)
(struct expr-if   (type tst thn els) #:prefab)
(struct expr-app  (type rator rands) #:prefab)
(struct expr-val  (type v) #:prefab)
(struct expr-intr (sym) #:prefab)
(struct expr-intrf (sym) #:prefab)

(struct reducer-split (e a b) #:prefab)
(struct reducer-fanout (a b) #:prefab)
(struct reducer-add (i) #:prefab)
(struct reducer-nop () #:prefab)
(struct reducer-index (i e b) #:prefab)

(struct pat-true ())
(struct pat-false ())
(struct pat-pair (p))
(struct pat-var ())
(struct pat-ident ())

(define internal-ops
  (apply set '(size index recip nat2prob prob2real + * == and < or not
                    categorical)))
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
     t]
    [(expr-bucket t s e b)
     t]))
