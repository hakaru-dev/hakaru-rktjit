#lang racket
(require ffi/unsafe)
(require rackunit)
(require sham/jit)
(require "../private/jit.rkt")
(require "../private/utils.rkt")
(require "../private/jit-utils.rkt")
(require "example-vectors.rkt")

(define src (read-file "../hkr/summate-prob.hkr"))
(define mod-env (compile-src src))

(define main (jit-get-function 'main mod-env))
(hakaru-defines mod-env)
(check-=
 (prob->real (main (make-c-array-prob small99) 10))
 (foldl + (* 10 99) (vector->list  (vector-map prob->real small99)))
 0.0000001)
