#lang racket
(require "ast.rkt")
(provide (all-defined-out))

;; if the expr has a fold somewhere
(define (is-complex? expr)
  (match expr
    [(expr-sum _ _ _ _ _) #t]
    [(expr-prd _ _ _ _ _) #t]
    [(expr-arr _ _ _ _) #t]
    [(expr-bucket _ _ _ _) #t]
    [(expr-let _ _ v b) (or (is-complex? v) (is-complex? b))]
    [(expr-match _ _ brs) (ormap is-complex? brs)]
    [(expr-branch _ b) (is-complex? b)]
    [(expr-bind _ b) (is-complex? b)]
    [(expr-if _ tst thn els) (or (is-complex? tst) (is-complex? thn) (is-complex? els))]
    [(expr-app _ rt rds)
     (ormap is-complex? rds)]
    [(expr-var _ _ _)
     #f]
    [(expr-val _ _)
     #f]))

(define (find-free-variables expr)
  (define ffv^ find-free-variables)
  (match expr
    [(expr-let t var val b) (set-union (ffv^ val) (set-remove (ffv^ b) var))]
    [(expr-sum t i start end b) (set-union (ffv^ start) (ffv^ end) (set-remove (ffv^ b) i))]
    [(expr-prd t i start end b) (set-union (ffv^ start) (ffv^ end) (set-remove (ffv^ b) i))]
    [(expr-arr t i end b) (set-union (ffv^ end) (set-remove (ffv^ b) i))]
    [(expr-bucket t s e r) (set-union (ffv^ s) (ffv^ e) (ffv^ r))]
    [(expr-match t tst brns) (set-union (ffv^ tst) (apply set-union (map ffv^ brns)))]
    [(expr-branch pat b) (ffv^ b)]
    [(expr-if t tst thn els) (set-union (ffv^ tst) (ffv^ thn) (ffv^ els))]
    [(expr-app t rator rands) (apply set-union (map ffv^ rands))]
    [(expr-val t v) (seteqv)]
    [(expr-intr sym) (seteqv)]
    [(expr-var t s o) (seteqv expr)]
    [(expr-bind v e) (set-remove (ffv^ e) v)]

    [(reducer-split e a b) (set-union (ffv^ e) (ffv^ a) (ffv^ b))]
    [(reducer-fanout a b) (set-union (ffv^ a) (ffv^ b))]
    [(reducer-add i) (set-union (ffv^ i))]
    [(reducer-nop) (seteqv)]
    [(reducer-index i e b) (set-union (ffv^ i) (ffv^ e) (ffv^ b))]))
