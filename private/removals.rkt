#lang racket
(require "ast.rkt")

(provide remove-unit-lets
         remove-empty-lets
         remove-unused-lets)

(define remove-unit-lets
  (create-rpass
   (expr
    [(expr-let t var val body)
     (if (equal? (typeof val) 'unit)
         (begin
           (printf "removing: ~a as unit\n" (pe var))
           body)
         (expr-let t var val body))])
   (reducer)
   (stmt)
   (pat)))

(define remove-empty-lets
  (create-rpass
   (expr
    [(expr-let t var val body)
     #:when (equal? var body)
     (printf "replacing ~a with ~a\n" (pe var) (pe val))
     val])
   (reducer)
   (stmt)
   (pat)))

(define remove-unused-lets
  (create-rpass
   (expr
    [(expr-let t var val body)
     (define bff (find-free-variables body))
     (if (set-member? bff var)
         (expr-let t var val body)
         (begin
           (printf "removing: ~a=~a\n" (pe var) (pe body))
           body))])
   (reducer)
   (stmt)
   (pat)))
