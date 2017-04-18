#lang racket
(require ffi/unsafe)
(require "hakaru-jit.rkt")
(require "../racket-jit/jit.rkt")

(define src (read-file "examples/summate-nat.hkr"))
(define mod-env (compile-src src))

(define main (jit-get-function 'main mod-env))
(define prob-type (jit-get-racket-type (env-lookup 'prob mod-env)))
(define nat-type (jit-get-racket-type (env-lookup 'nat mod-env)))

(define make-array-prob (jit-get-function 'make-array-prob mod-env))
(define make-array-nat (jit-get-function 'make-array-nat mod-env))

(define nat-array
  (list->cblock '(14228 003821 030999 002363 024379
                           001317 002707 013426 000219 008970)
                nat-type))
(define arg1
  (make-array-prob 10 nat-array))
(define arg2 10)
(pretty-display (main arg1 arg2))

(define sp-ffi(ffi-lib "examples/libsumnat"))

(define-cstruct _ArrayProb ([size _int] [data _pointer]))
(define c-arg1 (make-ArrayProb 10 nat-array))
(define c-main (get-ffi-obj "fn_a" sp-ffi (_fun _ArrayProb nat-type -> nat-type)))
(pretty-display (c-main c-arg1 arg2))
