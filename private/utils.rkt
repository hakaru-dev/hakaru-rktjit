#lang racket

(require racket/splicing
         "ast.rkt")

(provide (all-defined-out))

(define debug-pass (make-parameter #f))
(define-syntax-rule (dprintf tst args ...)
  (when (and (debug-pass) tst) (printf args ...)))

(define-syntax-rule (dtprintf args ...) (dprintf #t args ...))

(define (symbol-append s1 s2) (string->symbol (format "~a~a" s1 s2)))

(define (change-orig-var v o)
  (set-expr-var-orig! v o))

(define csym '$)
(define msym 'm)
(define (set-mutable-var v) (change-orig-var v msym) v)
(define (set-constant-var v) (change-orig-var v csym) v)

(define (is-mutable-var? v) (equal? (expr-var-orig v) msym))
(define (is-constant-var? v) (equal? (expr-var-orig v) csym))

(define (wrap-expr typs vars vals s b)
  (cond [(list? vars) (expr-lets typs vars vals s b)]
        [(expr-var? vars) (expr-lets (list typs) (list vars) (list vals) s b)]
        [else (error "wrap-expr-unknown-vars" typs vars (pe vals) (ps s) (pe b))]))

(define (var-sym-append v t sym (o '_))
  (expr-var t (symbol-append (expr-var-sym v) sym) o))
