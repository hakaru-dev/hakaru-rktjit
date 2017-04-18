#lang racket
(require ffi/unsafe)
(require "hakaru-jit.rkt")
(require "../racket-jit/jit.rkt")

(define summate-prob-src (read-file "examples/summate-prob.hkr"))
(define mod-env (compile-src summate-prob-src))

(define main (jit-get-function 'main mod-env))
(define prob-type (jit-get-racket-type (env-lookup 'prob mod-env)))
(define nat-type (jit-get-racket-type (env-lookup 'nat mod-env)))

(define make-array-prob (jit-get-function 'make-array-prob mod-env))
(define make-array-nat (jit-get-function 'make-array-nat mod-env))

(define prob-array
  (list->cblock '(0.014228 0.003821 0.030999 0.002363 0.024379
                           0.001317 0.002707 0.013426 0.000219 0.008970)
                prob-type))
(define arg1
  (make-array-prob 10 prob-array))
(define arg2 1000)
(pretty-display (main arg1 arg2))

(define sp-ffi(ffi-lib "examples/libsumprob"))

(define-cstruct _ArrayProb ([size _int] [data _pointer]))
(define c-arg1 (make-ArrayProb 10 prob-array))
(define c-main (get-ffi-obj "fn_a" sp-ffi (_fun _ArrayProb nat-type -> prob-type)))
(pretty-display (c-main c-arg1 arg2))
