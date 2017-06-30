#lang racket
(require "ast.rkt")
(require "pass-utils.rkt")

(provide remove-unit-lets
         remove-empty-lets
         remove-unused-lets)

(define remove-unit-lets
  (create-rpass
   (expr
    [(expr-let t var val body)
     (if (equal? (typeof val) 'unit)
         body
         (expr-let t var val body))])
   (reducer)
   (stmt)
   (pat)))

(define remove-empty-lets
  (create-rpass
   (expr
    [(expr-let t var val body)
     (if (eq? var body)
         (begin
           (printf "removing: ~a from: ~a\n" var (print-expr body))
           val)
         (expr-let t var val body))])
   (reducer)
   (stmt)
   (pat)))

(define remove-unused-lets
  (create-rpass
   (expr
    [(expr-let t var val body)
     (define bff (find-free-variables body))
     (if (set-member? bff (expr-var-sym var))
         (begin
           (printf "removing: ~a\n" (pe var))
           (expr-let t var val body))
         body)])
   (reducer)
   (stmt)
   (pat)))
