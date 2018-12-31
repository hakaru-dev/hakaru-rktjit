#lang racket
(require sham
         "type-defines.rkt"
         "basic-defines.rkt"
         "array-defines.rkt"
         "pair-defines.rkt"
         "struct-defines.rkt"
         "probability-defines.rkt")
(require "defs.rkt")
(require ffi/unsafe)
(define a8s `(array real (size . 8)))
(define a10s `(array real (size . 10)))
(define ap3s `(array prob (size . 3)))

(define sts `(struct-type (,a8s ,a10s)))
(define-values (rtr0 def0) (build-struct-index 'index.0  a8s (list sts)))
(define-values (rtr1 def1) (build-struct-index 'index.1  a10s (list sts)))
(define-values (rtr2 def2) (build-categorical ap3s))

(define defs
  (append
   (basic-defs)
   (probability-defs)
   (append-map array-defs
               `(;(array nat)
                 ;(array real)
                 (array prob)))
   (append-map const-array-defs (list ap3s)) ;a8s a10s ap3s))
   ;(get-struct-defs sts)
   (list ;(build-array-literal `(array real) 3)
    ;def0 def1
    def2)))
(define mod
  (sham:module (basic-mod-info) (cleanup-defs defs)))
(define cmod (compile-module mod))
(jit-dump-module cmod)
(jit-verify-module cmod)
(initialize-jit! cmod)
(define (get-f s)
  (jit-get-function s cmod))

;; (define ns8 (get-f 'new-sized$array<8.real>))
;; (define gs8 (get-f 'get-size$array<8.real>))
;; (define gi8 (get-f 'get-index$array<8.real>))
;; (define li8 (get-f 'literal$array<8.real>))
;; (define si8 (get-f 'set-index!$array<8.real>))

;; (define ns10 (get-f 'new-sized$array<10.real>))
;; (define gs10 (get-f 'get-size$array<10.real>))
;; (define gi10 (get-f 'get-index$array<10.real>))
;; (define li10 (get-f 'literal$array<10.real>))
;; (define si10 (get-f 'set-index!$array<10.real>))

(define nsp3 (get-f 'new-sized$array<3.prob>))
(define gsp3 (get-f 'get-size$array<3.prob>))
(define gip3 (get-f 'get-index$array<3.prob>))
(define lip3 (get-f 'literal$array<3.prob>))
(define sip3 (get-f 'set-index!$array<3.prob>))

(define categorical-prob3 (get-f 'categorical$array<3.prob>))
(define categorical-prob (get-f 'categorical))

(define t-real (jit-get-racket-type 'real cmod))
(define t-prob (jit-get-racket-type 'prob cmod))

(define c-real2prob (get-f 'real2prob))
(define make-array-prob (get-f 'make$array<prob>))

(define tr (list (c-real2prob 0.034)
                 (c-real2prob 0.039)
                 (c-real2prob 0.038)
                 (c-real2prob 0.035)))
(define test-prob (make-array-prob (length tr) (list->cblock tr t-prob)))
(define init-rng (get-f 'init-rng))
(init-rng)

(printf "categorical-prob: ~a\n" (categorical-prob test-prob))
;; ;<{ [8 x double], [10 x double] }>
;; (define ms  (get-f 'make$struct<array<8.real>.array<10.real>>))
;; (define ls (get-f 'literal$struct<array<8.real>.array<10.real>>))
;; (define si0 (get-f 'index$struct<array<8.real>.array<10.real>>.0))
;; (define si1 (get-f 'index$struct<array<8.real>.array<10.real>>.1))

(define ap3  (lip3 (c-real2prob 0.034)
                   (c-real2prob 0.039)
                   (c-real2prob 0.038)))
(gip3 ap3 0)
(gip3 ap3 1)
(gip3 ap3 2)
(categorical-prob3 ap3)
;; (define a8 (li8 0.0 1.0 2.0 3.0 4.0 5.0 6.0 7.0))
;; (define a10 (li10 0.0 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0))

;; (printf "a8: ")
;; (for ([i (in-range 8)])
;;   (printf "~a, " (gi8 a8 i)))
;; (printf "\n")

;; (printf "a10: ")
;; (for ([i (in-range 10)])
;;   (printf "~a, " (gi10 a10 i)))
;; (printf "\n")

;; (define s (ls a8 a10))

;; (define a8-s (si0 s))
;; (define a10-s (si1 s))

;; (printf "a8: ")
;; (for ([i (in-range 8)])
;;   (printf "~a, " (gi8 a8-s i)))
;; (printf "\n")

;; (printf "a10: ")
;; (for ([i (in-range 10)])
;;   (printf "~a, " (gi10 a10 i)))
;; (printf "\n")
