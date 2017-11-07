#lang racket

(require racket/splicing
         "ast.rkt")

(provide (all-defined-out))

(define debug-pass (make-parameter #f))
(define-syntax-rule (dprintf tst args ...)
  (when (and (debug-pass) tst)
    (printf args ...)))

(define-syntax-rule (dtprintf args ...)
  (dprintf #t args ...))

(define (symbol-append s1 s2)
  (string->symbol
   (string-append (symbol->string s1)
                  (symbol->string s2))))

(define (change-orig-var v o)
  (set-expr-var-orig! v o))

(define csym '$)
(define msym 'm)
(define (set-mutable-var v)
  (change-orig-var v msym))
(define (set-constant-var v)
  (change-orig-var v csym))

(define (is-mutable-var? v)
  (equal? (expr-var-orig v) msym))
(define (is-constant-var? v)
  (equal? (expr-var-orig v) csym))

;; (define (wrap-stmt vars vals stmt)
;;   (cond [(list? vars) (stmt-elets vars vals stmt)]
;;         [(empty? vars) stmt]
;;         [(expr-var? vars) (stmt-elets (list vars) (list vals) stmt)]))

(define (wrap-expr typs vars vals s b)
  (cond [(list? vars) (expr-lets typs vars vals s b)]
        [(expr-var? vars) (expr-lets (list typs) (list vars) (list vals) s b)]))

(define (var-sym-append v t sym (o '_))
  (expr-var t (symbol-append (expr-var-sym v) sym) o))
