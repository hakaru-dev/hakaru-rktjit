#lang racket

(define (folds->for expr)
  (define fns (box '()))
  (define (add-to-fns fn)
    (set-box! fns (cons fn (unbox fns))))
  (define pass
    (create-rpass
     (expr [(expr-arr t index size body)
            (define fvars (set->list (find-free-variables body)))
            (define fname (gensym^ 'arrf))
            (add-to-box (expr-fun fname fvars t (do-arr index size body)))
            (expr-app t (expr-intrf fname) fvars)]
           [(expr-sum t index start end body)
            (define fvars (set->list (find-free-variables body)))
            (define fname (gensym^ 'sumf))
            (add-to-box (expr-fun fname fvars t (do-sum index start end body)))
            (expr-app t (expr-intrf fname) fvars)]
           [(expr-prd t index start end body)
            (define fvars (set->list (find-free-variables body)))
            (define fname (gensym^ 'prdf))
            (add-to-box (expr-fun fname fvars t (do-sum index start end body)))
            (expr-app t fname fvars)])
     (reducer)
     (stmt)
     (pat)))
  (define new-mod (pass expr))
  (expr-mod (expr-mod-main new-mod) (append (expr-mod-fns new-mod) (unbox fns))))

(define (do-arr t index size body)
  (define result (expr-var t (gensym^ 'arr) '_))
  (define new-array (expr-app t (expr-intrf (symbol-append 'empty- (get-print-type t)))
                              size))
  (expr-let
   t result new-array
   (expr-block t
    (stmt-for index 0 size
     (stmt-assign (expr-app (expr-intr 'index) (list result index)) body))
    result)))
