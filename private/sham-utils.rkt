#lang racket
(require sham/jit
         sham/ast)

(provide (all-defined-out))

(define pointer-format "~a*")
(define array-format "array<~a>")
(define measure-format "measure<~a>")

(define type-nat (sham:type:ref 'nat))
(define type-real (sham:type:ref 'real))
(define type-prob (sham:type:ref 'prob))

(define (real-value v)
  (sham:exp:fl-value v type-real))
(define (nat-value v)
  (sham:exp:ui-value v type-nat))
