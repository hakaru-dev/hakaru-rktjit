#lang racket

(provide (all-defined-out))

(define pointer-format "~a*")
(define array-format "array<~a>")
(define array-args '(size data))

(define pair-format "pair<~a,~a>")
(define pair-car-sym 'a)
(define pair-cdr-sym 'b)
(define make-pair-fun-format "make<~a>")
(define pair-car-fun-format "car<~a>")
(define pair-cdr-fun-format "cdr<~a>")

(define make-array-fun-format "make<~a>")
(define get-array-data-fun-format "get-data<~a>")
(define get-array-size-fun-format "get-size<~a>")
(define new-size-array-fun-format "new-sized<~a>")
(define get-index-fun-format "get-index<~a>")
(define set-index-fun-format "set-index<~a>")
(define empty-array-fun-format "empty<~a>")

(define add-fun-format "add-~a-~a");;add-<num-args>-<type>
(define mul-fun-format "mul-~a-~a");;mul-<num-args>-<type>
(define recip-fun-format "recip-~a")
