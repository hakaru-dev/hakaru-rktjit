#lang racket
(require "ast.rkt")
(require "pass-utils.rkt")

(provide remove-unit-lets
         remove-empty-lets
         remove-unused-lets)

(define remove-unit-lets
  (expr/pass
   [(expr-let t var val body)
    (if (equal? (typeof val) 'unit)
        body
        (expr-let t var val body))]))

(define remove-empty-lets
  (expr/pass
   [(expr-let t var val body)
    (if (eq? var body)
        (begin
          (printf "removing: ~a from: ~a\n" var (print-expr body))
          val)
        (expr-let t var val body))]))


(define remove-unused-lets
  (expr/pass
   [(expr-let t var val body)
    (define bff (find-free-variables body))
    (printf "let: ~a, ~a\n" (print-expr var) (set-member? bff var))
    (if (set-member? bff var)
        (expr-let t var val body)
        body)]))
