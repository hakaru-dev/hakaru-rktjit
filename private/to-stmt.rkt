#lang racket
(require "ast.rkt"
         "utils.rkt")
(provide folds->for
         to-stmt)



(define (folds->for e)
  (define fns (box '()))
  (define (add-to-fns fn)
    (set-box! fns (cons fn (unbox fns))))
  (define pass
    (create-rpass
     (expr [(expr-arr t index size body)
            (define fvars (set->list (set-remove (find-free-variables body) index)))

            (define fname (gensym^ 'af))
            (add-to-fns (cons fname (expr-fun fvars t (do-arr t index size body))))
            (expr-app t (expr-intrf fname) fvars)]
           [(expr-sum t index start end body)
            (define fvars (set->list (set-remove (find-free-variables body) index)))
            (define fname (gensym^ 'sf))
            (add-to-fns (cons fname (expr-fun fvars t (do-sum t index start end body))))
            (expr-app t (expr-intrf fname) fvars)]
           [(expr-prd t index start end body)
            (define fvars (set->list (set-remove (find-free-variables body) index)))
            (define fname (gensym^ 'pf))
            (add-to-fns (cons fname (expr-fun fvars t (do-prd t index start end body))))
            (expr-app t (expr-intrf fname) fvars)])
     (reducer)
     (stmt)
     (pat)))
  (define new-mod (pass e))
  (expr-mod (expr-mod-main new-mod) (append (expr-mod-fns new-mod) (unbox fns))))

(define (do-arr t index size body)
  (make-fold t
             (expr-var t (gensym^ 'ar) '_)
             (expr-app t (expr-intrf (symbol-append 'empty- (get-print-type t)))
                       (list size))
             index (expr-val 0 'nat) size
             (λ (index result) (stmt-assign (expr-app t (expr-intr 'index)
                                                      (list result index)) body))))

(define (do-sum t index start end body)
  (make-fold t
             (expr-var t (gensym^ 'sr) '_)
             (expr-val 0 t)
             index start end
             (λ (index result) (stmt-assign result (expr-app t (expr-intr '+)
                                                             (list result body))))))

(define (do-prd t index start end body)
  (make-fold t
             (expr-var t (gensym^ 'pr) '_)
             (expr-val 1 t)
             index start end
             (λ (index result) (stmt-assign result (expr-app t (expr-intr '*)
                                                             (list result body))))))

(define (make-fold t result init-value index start end body-gen)
  (expr-let t result init-value
   (expr-block t
    (stmt-for index start end
     (body-gen index result))
    result)))

(define (mod-stmt m)
  (match m
    [(expr-mod main fns)
     (expr-mod (fn-stmt main) (map (λ (p) (cons (car p) (fn-stmt (cdr p)))) fns))]))
(define (fn-stmt f)
  (match f
    [(expr-fun args ret-type body)
     (if (expr? body)
         (expr-fun args ret-type (expr->ret-stmt body))
         f)]))
(define (expr->ret-stmt e)
  (define ers expr->ret-stmt)
  (match e
    [(expr-if typ tst thn els)
     (stmt-if tst (ers thn) (ers els))]
    [(expr-let t var val body)
     (stmt-lets (list var)
                (stmt-block (list (stmt-assign var val)
                                  (ers body))))]
    [(expr-lets t vars vals body)
     (stmt-lets (list vars)
                (stmt-block (append
                             (for/list [(var vars) (val vals)]
                               (stmt-assign var val))
                             (list (ers body)))))]
    [(expr-block t stmt body)
     (stmt-block (list stmt (ers body)))]
    [else (stmt-return e)]))
;; Converts function body to stmt
(define to-stmt mod-stmt)
