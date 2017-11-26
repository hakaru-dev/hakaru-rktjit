#lang racket
(require ffi/unsafe
         racket/runtime-path)

(require "../../jit.rkt"
         sham)

(define input-3-10 '([3.8728103253204136
                      1.1452918218810444
                      -0.37443733246614497
                      2.2524280674567634
                      0.1088871787126991
                      2.2484645323958334
                      0.19013878436498044
                      1.4032911741452248
                      2.1930977191694936
                      1.7312282946567383]
                     .
                     [2 2 2 2 1 2 2 2 2 2]))

(define empty-info-3-10 '(() () () () () ()))
(define-runtime-path current-dir "./")

(define partial1-env (compile-file (build-path current-dir "partial1.hkr") empty-info-3-10))

;; (jit-dump-module partial1-env)

(define (run-test module-env input output)
  (define points (length (car input)))
  (define classes (add1 (apply max (cdr input))))
  (printf "running test p: ~a, c: ~a\n" points classes)

  (define prog (jit-get-function 'prog module-env))

  (define make-prob-array      (jit-get-function (string->symbol (format "make$array<prob>")) module-env))
  (define new-sized-prob-array (jit-get-function (string->symbol (format "new-sized$array<prob>")) module-env))
  (define free-prob-array      (jit-get-function (string->symbol (format "free-sized$array<prob>")) module-env))
  (define set-index-prob-array (jit-get-function (string->symbol (format "set-index!$array<prob>")) module-env))
  (define get-index-prob-array (jit-get-function (string->symbol (format "get-index$array<prob>")) module-env))
  (define get-size-prob-array (jit-get-function (string->symbol (format "get-size$array<prob>")) module-env))


  (define make-nat-array      (jit-get-function (string->symbol (format "make$array<nat>")) module-env))
  (define new-sized-nat-array (jit-get-function (string->symbol (format "new-sized$array<nat>")) module-env))
  (define free-nat-array      (jit-get-function (string->symbol (format "free-sized$array<nat>")) module-env))
  (define set-index-nat-array (jit-get-function (string->symbol (format "set-index!$array<nat>")) module-env))
  (define get-index-nat-array (jit-get-function (string->symbol (format "get-index$array<nat>")) module-env))
  (define get-size-nat-array (jit-get-function (string->symbol (format "get-size$array<nat>")) module-env))


  (define make-real-array      (jit-get-function (string->symbol (format "make$array<real>")) module-env))
  (define new-sized-real-array (jit-get-function (string->symbol (format "new-sized$array<real>")) module-env))
  (define free-real-array      (jit-get-function (string->symbol (format "free-sized$array<real>")) module-env))
  (define set-index-real-array (jit-get-function (string->symbol (format "set-index!$array<real>")) module-env))
  (define get-index-real-array (jit-get-function (string->symbol (format "get-index$array<real>")) module-env))
  (define get-size-real-array  (jit-get-function (string->symbol (format "get-size$array<real>")) module-env))


  (define real2prob (jit-get-function (string->symbol "real2prob") module-env))
  (define prob2real (jit-get-function (string->symbol "prob2real") module-env))

  (define stdev (real2prob 14.0))

  (define as (new-sized-prob-array classes))
  (for ([i (in-range classes)])
    (set-index-prob-array as i 0.0))

  (define (make-zs lst)
    (define arr (new-sized-nat-array (length lst)))
    (for ([v lst]
          [i (in-range (length lst))])
      (set-index-nat-array arr i  v))
    arr)

  (define (make-ts lst)
    (define arr (new-sized-real-array (length lst)))
    (for ([v lst]
          [i (in-range (length lst))])
      (set-index-real-array arr i (exact->inexact v)))
    arr)

  (define ts (make-ts (car input)))
  (define zs (make-zs (cdr input)))
  (define doc 0)

  (define output-c (prog stdev as zs ts doc))
  (define output-list
    (for/list ([i (get-size-prob-array output-c)])
      (prob2real (get-index-prob-array output-c i))))
  (printf "output from prog: ~a\n" output-list)
  (define output-hs (list 322.48561494234417  190.89773000645684 2345.667007610382))
  (printf "output from haskell: ~a\n" output-hs))

(run-test partial1-env input-3-10 '())
