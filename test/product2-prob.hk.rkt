#lang racket
(require ffi/unsafe)
(require sham/jit)
(require rackunit)
(require "../private/utils.rkt")
(require "../private/jit.rkt")
(require "../private/jit-utils.rkt")
(require "example-vectors.rkt")

(define src (read-file "../hkr/product2-prob.hkr"))
(define mod-env (compile-src src))

(define main (jit-get-function 'main mod-env))
(hakaru-defines mod-env)
(define arg1 (vector 1.0 2.0))
(define arg2 (vector 3.0 4.0))
(check-=
 (prob->real (main
              (make-c-array-prob (vector-map real->prob arg1))
              (make-c-array-prob (vector-map real->prob arg2))))
 (for/fold [(s 1)]
           [(a arg1)]
   (* s
      (for/fold [(p 1)]
              [(b arg2)]
        (* p (* a b)))))
 0.0000001)
