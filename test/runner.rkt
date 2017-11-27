#lang racket
(require ffi/unsafe
         racket/runtime-path)

(require "../jit.rkt"
         sham)

(define-runtime-path current-dir "./")


(define zp-info (list '()))
(define zp-env (compile-file (build-path current-dir "zero-product.hkr") zp-info ))
(define real2prob (jit-get-function (string->symbol "real2prob") zp-env))
(define prob2real (jit-get-function (string->symbol "prob2real") zp-env))
(define zp-prog (jit-get-function 'prog zp-env))
(prob2real (zp-prog 0.0))
