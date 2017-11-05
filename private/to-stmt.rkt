#lang racket
(require "ast.rkt"
         "utils.rkt")
(provide to-stmt
         expr->stmt
         simplify-set
         remove-if-expr)

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
       (stmt-elets (list var) (list val) (ers body))]
      [(expr-lets t vars vals body)
       (stmt-elets vars vals (ers body))]
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
    [(stmt-assign lhs rhs)
     #:when (expr-app? lhs)
     (stmt-elets
      (list (expr-var 'void '_ '_))
      (list (match lhs
              [(expr-app t (expr-intrf 'car) args)
               (expr-app 'void (expr-intrf 'set-car!) (append args (list rhs)))]
              [(expr-app t (expr-intrf 'cdr) args)
               (expr-app 'void (expr-intrf 'set-cdr!) (append args (list rhs)))]
              [(expr-app t (expr-intrf 'index) args)
               (expr-app 'void (expr-intrf 'set-index!) (append args (list rhs)))]))
      (stmt-void))]
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
                                   (expr->stmt (expr-if t tst thn els)
                                               (λ (e) (stmt-return e))))))
       (expr-app t (expr-intrf fname) fvars)])
     (reducer)
     (stmt)
     (pat)))
  (define new-mod (pass m))
  (expr-mod (expr-mod-main new-mod) (append (expr-mod-fns new-mod) (unbox fns))))
