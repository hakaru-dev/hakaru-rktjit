#lang racket
(require "ast.rkt")
(provide simplify-match
         simplify-lets
         )

(define simplify-match
  (create-rpass
   (expr [(expr-match t tst brs)
          (if (eq? (typeof tst) 'bool) (toif t tst brs) (extract-pair t tst brs))])
   (reducer)
   (stmt)
   (pat)))

(define (toif t tst brs)
  (define-values (tb fb)
    (if (pat-true? (expr-branch-pat (car brs)))
        (values (car brs) (cadr brs))
        (values (cadr brs) (car brs))))
  (expr-if t tst (expr-branch-body tb) (expr-branch-body fb)))

(define (extract-pair t tst brs)
  (unless (and (eq? (length brs) 1)
               (eq? (car (typeof tst)) 'pair)
               (pat-pair? (expr-branch-pat (car brs)))
               (pat-var? (pat-pair-a (expr-branch-pat (car brs))))
               (pat-var? (pat-pair-b (expr-branch-pat (car brs)))))
    (error "matching over pair with multiple branches or complex pattern." brs))

  (define car-type (cadr (typeof tst)))
  (define cdr-type (caddr (typeof tst)))
  (define car-bind (expr-branch-body (car brs)))
  (define cdr-bind (expr-bind-body car-bind))
  (define car-var (expr-bind-var car-bind))
  (define cdr-var (expr-bind-var cdr-bind))
  (printf "match-pair: \n\tbr: ~a\n\tat: ~a, bt: ~a\n"
          (pe (car brs))
          car-type cdr-type)
  (expr-let t car-var (expr-app car-type (expr-intrf 'car) (list tst))
            (expr-let t cdr-var (expr-app cdr-type (expr-intrf 'cdr) (list tst))
                      (expr-bind-body cdr-bind))))

(define (sl e env)
  (match e
    [(expr-let t v val body)
     (if (expr-var? val)
         (begin
           (printf "replacing: ~a with ~a\n" (print-expr v) (print-expr val))
           (sl body (hash-set env v val)))
         (expr-let t v (sl val env) (sl body env)))]
    [v #:when (hash-has-key? env v)
       (printf "\treplaced: ~a with ~a\n" (print-expr v) (print-expr (hash-ref env v)))
       (hash-ref env v)]
    [else (define f-expr (curryr sl env))
          (define f-stmt (λ (e) (map-stmt f-expr identity f-stmt identity e)))
          (map-expr f-expr identity f-stmt identity e)]))

(define (simplify-lets e)
  (sl e (make-immutable-hash)))