#lang racket
(require ffi/unsafe)
(require sham/jit)
(require "private/ast.rkt"
         "private/prelude/type-defines.rkt"
         "private/prelude/template-format.rkt")

(require (for-syntax racket/syntax))
(provide (all-defined-out))

(define (prob->real x) (exp x))
(define (real->prob x) (log x))
(define (nat->prob x) (real->prob (exact->inexact x)))

(define (logsumexp2 a b)
  (if (> a b)
      (+ a (log (exp (- b a))))
      (+ b (log (exp (- a b))))))

(define (one-of-type t)
  (if (equal? t 'prob)
      (real->prob 1.0)
      1.0))
(define (zero-of-type t)
  (if (equal? t 'prob)
      (real->prob 0.0)
      0.0))

(define logspace-add
  (λ args
    (real->prob (apply + (map prob->real args)))))

(define (replicate-vector n i)
  (build-vector n (const i)))

(define (read-vector-from-csv fname)
  (call-with-input-file fname
    (lambda (in)
      (for/vector [(s (in-lines in))]
        (string->number s)))))

(define (get-cmd-argument i)
  (vector-ref (current-command-line-arguments) i))

;; utils for running jit
(define (get-array-function-sym f t)
  (define ts (get-type-string t))
  (string->symbol
   (match f
     ['get-index (format get-index-fun-format ts)]
     ['get-size (format get-array-size-fun-format ts)]
     ['get-data (format get-array-data-fun-format ts)]
     ['set-index! (format set-index-fun-format ts)]
     ['make (format make-array-fun-format ts)]
     ['new (format new-size-array-fun-format ts)])))

(define (get-pair-function-sym f t)
  (define ts (get-type-string t))
  (string->symbol
   (match f
     ['make (format make-pair-fun-format ts)]
     ['car (format pair-car-fun-format ts)]
     ['cdr (format pair-cdr-fun-format ts)]
     ['set-car! (format pair-set-car-fun-format ts)]
     ['set-cdr! (format pair-set-cdr-fun-format ts)])))

(define (get-function module-env sym t)
  (jit-get-function (if (equal? (car t) 'pair)
                           (get-pair-function-sym sym t)
                           (get-array-function-sym sym t))
                       module-env))

(define (get-racket-type t)
  (match t
    ['real _double]
    ['prob _double]
    ['nat _uint64]
    ['unit _uint64]
    [else _pointer]))

(define (rkt->jit module-env type val)
  (match type
    [`(array ,t)
     (define size (length val))
     (define arr ((get-function module-env 'new type) size))
     (for ([j (in-range size)]
           [v val])
       ((get-function module-env 'set-index! type)
        arr j (rkt->jit module-env t v)))
     arr]
    [`(pair ,ta ,tb)
     (define tav (rkt->jit module-env ta (car val)))
     (define tbv (rkt->jit module-env tb (cdr val)))
     ((get-function module-env 'make type) tav tbv)]
    ['prob ((jit-get-function 'real2prob module-env) (exact->inexact val))]
    ['real (exact->inexact val)]
    [else val]))

(define (jit->rkt module-env type val)
  (match type
    [`(array ,t)
     (define size ((get-function module-env 'get-size type) val))
     (define data ((get-function module-env 'get-data type) val))
     (for/list ([j (in-range size)])
       (jit->rkt module-env t ((get-function module-env 'get-index type) val j)))]
    [`(pair ,ta ,tb)
     (define tav
       (jit->rkt module-env ta ((get-function 'module-env 'car type) val)))
     (define tbv
       (jit->rkt module-env tb ((get-function 'module-env 'cdr type) val)))
     (cons tav tbv)]
    [`(pointer ,t)
     (jit->rkt module-env t val)]
    ['prob ((jit-get-function 'prob2real module-env) val)]
    [else val]))
