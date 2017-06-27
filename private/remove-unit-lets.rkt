#lang racket
(require "ast.rkt")

(provide remove-unit-lets
         remove-empty-lets)

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


;; (define remove-unused-lets
;;   (expr/pass
;;    [(expr-let t var val body)
;;     (define bff (find-free-variables body))
;;     (if (set-member? var bff)
;;         (expr-let t var val)
;;         body)]))
