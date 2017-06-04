#lang racket
(require ffi/unsafe)
(require sham/jit)
(require "../private/jit.rkt")
(require "../private/jit-utils.rkt")
(require "example-vectors.rkt")

(define src (read-file "../hkr/nb_simp.hkr"))
;; (define mod-env (compile-src src))

;; (define main (jit-get-function 'main mod-env))
;; (hakaru-defines mod-env)
