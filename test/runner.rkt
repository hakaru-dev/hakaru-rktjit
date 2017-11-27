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

(define pp-env (compile-file (build-path current-dir "prob-product.hkr") zp-info ))
(define make-prob-array      (jit-get-function (string->symbol (format "make$array<prob>")) pp-env))
(define new-sized-prob-array (jit-get-function (string->symbol (format "new-sized$array<prob>")) pp-env))
(define free-prob-array      (jit-get-function (string->symbol (format "free-sized$array<prob>")) pp-env))
(define set-index-prob-array (jit-get-function (string->symbol (format "set-index!$array<prob>")) pp-env))
(define get-index-prob-array (jit-get-function (string->symbol (format "get-index$array<prob>")) pp-env))
(define get-size-prob-array (jit-get-function (string->symbol (format "get-size$array<prob>")) pp-env))
(define (make-c-prob-array lst)
  (define arr (new-sized-prob-array (length lst)))
  (for ([v lst]
        [i (in-range (length lst))])
    (set-index-prob-array arr i (real2prob v)))
  arr)
(define pp-prog (jit-get-function 'prog pp-env))
(prob2real (pp-prog (make-c-prob-array '(13.0 14.0))))
