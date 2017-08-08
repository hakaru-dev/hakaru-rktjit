#lang racket
(require "ast.rkt"
         "utils.rkt")

(define (folds->for e)
  (define fns (box '()))
  (define (add-to-fns fn)
    (set-box! fns (cons fn (unbox fns))))
  (define pass
    (create-rpass
     (expr [(expr-arr t index size body)
            (define fvars (set->list (find-free-variables body)))
            (define fname (gensym^ 'arrf))
            (add-to-fns (expr-fun fname fvars t (do-arr index size body)))
            (expr-app t (expr-intrf fname) fvars)]
           [(expr-sum t index start end body)
            (define fvars (set->list (find-free-variables body)))
            (define fname (gensym^ 'sumf))
            (add-to-fns (expr-fun fname fvars t (do-sum index start end body)))
            (expr-app t (expr-intrf fname) fvars)]
           [(expr-prd t index start end body)
            (define fvars (set->list (find-free-variables body)))
            (define fname (gensym^ 'prdf))
            (add-to-fns (expr-fun fname fvars t (do-sum index start end body)))
            (expr-app t fname fvars)]
           )
     (reducer)
     (stmt)
     (pat)))
  (define new-mod (pass e))
  (expr-mod (expr-mod-main new-mod) (append (expr-mod-fns new-mod) (unbox fns))))

(define (do-arr t index size body)
  (make-fold t
             (expr-var t (gensym^ 'arr) '_)
             (expr-app t (expr-intrf (symbol-append 'empty- (get-print-type t)))
                              size)
             index (expr-val 0 'nat) size
             (λ (index result) (stmt-assign (expr-app (expr-intr 'index) (list result index)) body))))

(define (do-sum t index start end body)
  (make-fold t
             (expr-var t (gensym^ 'sum) '_)
             (expr-val 0 t)
             index start end
             (λ (index result) (stmt-assign result (expr-app (expr-intr '+) result body)))))

(define (do-prd t index start end body)
  (make-fold t
             (expr-var t (gensym^ 'prd) '_)
             (expr-val 1 t)
             index start end
             (λ (index result) (stmt-assign result (expr-app (expr-intr '*) result body)))))

(define (make-fold t result init-value index start end body-gen)
  (expr-let t result init-value
   (expr-block t
    (stmt-for index start end
     (body-gen index result))
    result)))
