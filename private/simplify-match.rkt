#lang racket
(require "ast.rkt")
(provide simplify-match)

(define simplify-match
  (expr/pass
   [(expr-match t tst brs)
    (if (eq? (typeof tst) 'bool) (toif t tst brs) (extract-pair t tst brs))]))

(define (toif t tst brs)
  (define-values (tb fb)
    (if (pat-true? (expr-branch-pat (car brs)))
        (values (car brs) (cadr brs))
        (values (cadr brs) (car brs))))
  (expr-if t tst (expr-branch-body tb) (expr-branch-body fb)))

(define (extract-pair t tst brs)
  (unless (and (eq? (length brs) 1)
               (eq? (car (typeof tst)) 'pair)
               (equal? (expr-branch-pat (car brs)) (pat-pair (list (pat-var) (pat-var)))))
    (error "matching over pair with multiple branches or complex pattern." brs))

  (printf "match pair")
  (printf "pat: ~a\n" (expr-branch-pat (car brs)))
  (define car-type (cadr (typeof tst)))
  (define cdr-type (caddr (typeof tst)))
  (printf "car-type: ~a, cdr-type: ~a\n" car-type cdr-type)
  (printf "branch: ~a\n" (print-expr (car brs)))
  (define car-bind (expr-branch-body (car brs)))
  (define cdr-bind (expr-bind-body car-bind))
  (define car-var (expr-bind-var car-bind))
  (define cdr-var (expr-bind-var cdr-bind))
  (printf "car-var: ~a, cdr-var: ~a\n" (print-expr car-var) (print-expr cdr-var))
  (expr-let t car-var (expr-app car-type (expr-intrf 'car) (list tst))
            (expr-let t cdr-var (expr-app cdr-type (expr-intrf 'cdr) (list tst))
                      (expr-bind-body cdr-bind))))
