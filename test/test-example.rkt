#lang racket
(require math/flonum)
(require ffi/unsafe)
(require "hakaru-jit.rkt")
(require "../racket-jit/jit.rkt")

(define summate-prob-src (read-file "examples/summate-prob.hkr"))
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
(define (make-c-prob-array prob-vector)
  (make-array-prob (vector-length prob-vector)
                   (vector->cblock prob-vector prob-type)))
(define make-array-nat (jit-get-function 'make-array-nat mod-env))
(define (make-c-nat-array nat-vector)
  (make-array-nat (vector-length nat-vector)
                  (vector->cblock nat-vector nat-type)))

(define get-array-prob (jit-get-function 'get-array-prob mod-env))
(define get-array-nat (jit-get-function 'get-array-nat mod-env))

(define size-array-prob (jit-get-function 'size-array-prob-p mod-env))
(define size-array-nat (jit-get-function 'size-array-nat-p mod-env))
(define (get-prob-vector cs)
  (cblock->vector (get-array-prob cs) prob-type (size-array-prob cs)))
(define (get-nat-vector cs)
  (cblock->vector (get-array-nat cs) nat-type (size-array-nat cs)))

(define p-a '(5.0 10.0))

(define prob-array
  (list->cblock p-a
                prob-type))
(define prob-array-l
  (list->cblock (map real2prob p-a)
                prob-type))
(define arg1
  (make-array-prob (length p-a) prob-array))
(define arg2 0)
(printf "jit-output: ~a\n" (main arg1 arg2))

(define sp-ffi (ffi-lib "examples/libsumprob"))

(define-cstruct _ArrayProb ([size _int] [data _pointer]))
(define c-arg1 (make-ArrayProb (length p-a) prob-array-l))

(define c-main (get-ffi-obj "fn_a" sp-ffi (_fun _ArrayProb nat-type -> prob-type)))
(define c-out (c-main c-arg1 arg2))
(printf "c-output: ~a, prob2real: ~a\n" c-out (prob2real c-out))

#|
>:l examples/summate-prob.hs
>import Data.Number.LogFloat as LF
>import Data.Vector.Unboxed as UV
>import Data.Vector as V
>prog (UV.fromList [(LF.logFloat 5.0), (LF.logFloat 10.0)]) 0
--logFloat 15.0
> prog (UV.fromList [(LF.logToLogFloat 1.609438), (LF.logToLogFloat 2.302585)]) 0
--logFloat 14.999999507889102
> prog (UV.fromList [(LF.logToLogFloat 1.6094379124341003), (LF.logToLogFloat 2.302585092994046)]) 0
--logFloat 15.0
> LF.logFromLogFloat 15.0
2.70805020110221

|#
;Diagrams.Backend.CmdLine
