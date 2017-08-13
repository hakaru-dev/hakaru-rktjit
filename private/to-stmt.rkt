#lang racket
(require "ast.rkt"
         "utils.rkt")
(provide folds->for
         to-stmt
         simplify-set
         remove-if-expr)

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
            (define fn (expr-fun fvars t (do-prd t index start end body)))
            (add-to-fns (cons fname fn))
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
  (make-fold
   t
   (expr-var t (gensym^ 'pr) '_)
   (expr-val 1 t)
   index start end
   (λ (index result)
     (expr->stmt body (λ (e)
                        (stmt-assign result (expr-app t (expr-intr '*)
                                                      (list result e))))))))

;; create a fold expression with the function body-gen being called with index and result
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

;; converts an expression to statment with the value given to
;; assign-to function at the end
(define (expr->stmt e assign-to)
  (define (ers e)
    (match e
      [(expr-if typ tst thn els)
       (stmt-if tst (ers thn) (ers els))]
      [(expr-let t var val body)
       (stmt-lets (list var)
                  (stmt-block (list (stmt-assign var val)
                                    (ers body))))]
      [(expr-lets t vars vals body)
       (stmt-lets vars
                  (stmt-block (append
                               (for/list [(var vars) (val vals)]
                                 (stmt-assign var val))
                               (list (ers body)))))]
      [(expr-block t stmt body)
       (stmt-block (list stmt (ers body)))]
      [else (assign-to e)]))
  (ers e))

(define (expr->ret-stmt e)
  (expr->stmt e (λ (e) (stmt-return e))))

;; Converts function body to stmt with return of value
(define to-stmt mod-stmt)


;; 
(define simplify-set
  (create-pass
   (expr)
   (reducer)
   (stmt
    [(stmt-assign var val)
     (expr->stmt val (λ (e) (stmt-assign var e)))])
   (pat)))

(define (remove-if-expr m)
  (define fns (box '()))
  (define (add-to-fns fn)
    (set-box! fns (cons fn (unbox fns))))
  (define pass
    (create-rpass
     (expr
      [(expr-if t tst thn els)
       (define fvars (set->list (set-union (find-free-variables thn)
                                           (find-free-variables els)
                                           (find-free-variables tst))))
       (define fname (gensym^ 'ifun))
       (add-to-fns (cons fname
                         (expr-fun fvars t
                                   (expr->stmt (expr-if t tst thn els) (λ (e) (stmt-return e))))))
       (expr-app t (expr-intrf fname) fvars)])
     (reducer)
     (stmt)
     (pat)))
  (define new-mod (pass m))
  (expr-mod (expr-mod-main new-mod) (append (expr-mod-fns new-mod) (unbox fns))))
