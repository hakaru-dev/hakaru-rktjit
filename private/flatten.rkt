#lang racket

(require "ast.rkt")
(require "utils.rkt")

(provide flatten-anf)

(define (find-free-vars expr)
  (match expr
    [(expr-sum t i start end b)
     (define fbs (find-free-vars b))
     (set-union(find-free-vars start)
               (find-free-vars end)
               (set-remove
               (find-free-vars b)
                i))]
    [(expr-let t var val b)
     (set-union
      (find-free-vars val)
      (set-remove (find-free-vars b)
                  var))]
    [(expr-prd t i start end b)
     (set-union (find-free-vars start)
                (find-free-vars end)
                (set-remove
                 (find-free-vars b)
                 i))]
    [(expr-arr t i end b)
     (set-union(find-free-vars end)
               (set-remove
                (find-free-vars b)
                i))]
    [(expr-if t tst thn els)
     (set-union (find-free-vars tst)
                (find-free-vars thn)
                (find-free-vars els))]
    [(expr-app t rator rands)
     (apply set-union (map find-free-vars rands))]
    [(expr-val t v)
     (seteqv)]
    [(expr-intr sym)
     (seteqv)]
    [(expr-var t s o)
     (seteqv expr)]))

(define (flatten-anf expr)
  (define fns (box '()))
  (define (add-to-funs fn-name fn-expr)
    (set-box! fns (cons (cons fn-name fn-expr) (unbox fns))))
  
  (define (uf body)
    (match body
      [(expr-sum t i start end b)
       (define free-vars (set->list (find-free-vars body)))
       (define fn (expr-fun free-vars t (expr-sum t i start (uf end) (uf b))))
       (define fn-name (gensym^ 'fn))
       (add-to-funs fn-name fn)
       (expr-app t (expr-intr fn-name) free-vars)]
      [(expr-let type var val b)
       (expr-let type var (uf val) (uf b))]
      [(expr-prd t i start end b)
       (define free-vars (set->list (find-free-vars body)))
       (define fn (expr-fun free-vars t (expr-prd t i start (uf end) (uf b))))
       (define fn-name (gensym^ 'fn))
       (add-to-funs fn-name fn)
       (expr-app t (expr-intr fn-name) free-vars)]
      [(expr-arr t i end b)
       (define free-vars (set->list (find-free-vars body)))
       (define fn (expr-fun free-vars t (expr-arr t i (uf end) (uf b))))
       (define fn-name (gensym^ 'fn))
       (add-to-funs fn-name fn)
       (expr-app t (expr-intr fn-name) free-vars)]
      [(expr-if t tst thn els)
       (expr-if t (uf tst) (uf thn) (uf els))]
      [(expr-app t rt rds)
       (expr-app t rt (map uf rds))]
      [else body]))
  (match expr
    [(expr-fun args ret-type body)
     (let ((m (expr-fun args ret-type (uf body))))
       (begin
         (printf "fns: ~a\n" (print-expr m))
         (expr-mod m (unbox fns))))]))
