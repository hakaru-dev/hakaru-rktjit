#lang racket
(require math/flonum)
(require ffi/unsafe)
(require "hakaru-jit.rkt")
(require "../racket-jit/jit.rkt")

(define summate-prob-src (read-file "examples/sum-prob.hkr"))
(define mod-env (compile-src summate-prob-src))
(define (prob2real x) (+ (flexpm1 x) 1))
(define (real2prob x) (fllog1p (- x 1)))
(define (logsumexp2 a b)
  (if (> a b)
      (+ a (fllog1p (+ (flexpm1 (- b a)) 1)))
      (+ b (fllog1p (+ (flexpm1 (- a b)) 1)))))
(define main (jit-get-function 'main mod-env))
(define prob-type (jit-get-racket-type (env-lookup 'prob mod-env)))
(define nat-type (jit-get-racket-type (env-lookup 'nat mod-env)))

(define make-array-prob (jit-get-function 'make-array-prob mod-env))
(define make-array-nat (jit-get-function 'make-array-nat mod-env))

(define arg1 5.0)
(define arg2 10)
(printf "jit-output: ~a\n" (main arg1 arg2))

(define sp-ffi(ffi-lib "examples/sum-prob"))

(define c-main (get-ffi-obj "fn_a" sp-ffi (_fun prob-type nat-type -> prob-type)))

(printf "c-output: ~a\n" (prob2real (c-main (real2prob arg1) arg2)))
