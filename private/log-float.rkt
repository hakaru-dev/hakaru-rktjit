#lang racket

(require "utils.rkt")
(require "ast.rkt")
(provide insert-log-float)

(define (to-logspace rator)
  (expr-intr (symbol-append 'logspace- (expr-intr-sym rator))))
(define (cast-logspace rands)
  (map (λ (r)
         (if (equal? (typeof r) 'prob)
             r
             (expr-app 'tologspace
                       (expr-intr 'real2prob)
                       (list r))))
       rands))
(define (ilg ast)
  (match ast
    [(expr-app t rator rands)
     (if (and (expr-intr? rator)
              (member (expr-intr-sym rator) '(+ *))
              (member 'prob (map typeof rands)))
         (expr-app t (to-logspace rator) (cast-logspace rands))
         (expr-app t rator rands))]
    [else ast]))

(define (insert-log-float expr)
  (map-ast ilg expr))
