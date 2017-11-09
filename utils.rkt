#lang racket
(require ffi/unsafe)
(require sham/jit)

(require ffi/unsafe)
(require (for-syntax racket/syntax))
(provide (all-defined-out))

(define-syntax (hakaru-defines stx)
  (syntax-case stx ()
    [(_ me)
     #`(begin
         #,@(for/list ([t '(nat prob real)])
              (with-syntax ([ts (format-id stx "~a" t)]
                            [type (format-id stx "~a-type" t)]

                            [make-arrayf (format-id stx "make-array-~a" t)]
                            [make-array (format-id stx "make-array<~a>" t)]
                            [make-c-array (format-id stx "make-c-array-~a" t)]
                            [mac-v (format-id stx "~a-vector" t)]

                            [get-c-arrayf (format-id stx "get-array-~a" t)]
                            [get-c-array (format-id stx "get-ptr-array<~a>" t)]

                            [size-array (format-id stx "size-array-~a" t)]
                            [size-array-p (format-id stx "size-array<~a>" t)]

                            [get-vector (format-id stx "get-~a-vector" t)])
                #`(begin
                    (define type (jit-get-racket-type (env-lookup (quote ts) me)))
                    (define make-arrayf (jit-get-function (quote make-array) me))
                    (define (make-c-array mac-v)
                      (make-arrayf (vector-length mac-v)
                                  (vector->cblock mac-v type)))
                    (define get-c-arrayf (jit-get-function (quote get-c-array) me))
                    (define size-array (jit-get-function (quote size-array-p) me))
                    (define (get-vector cs)
                      (cblock->vector (get-c-arrayf cs) type (size-array cs)))))))]))



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

(define logspace-add (λ args (real->prob (apply + (map prob->real args)))))

(define (replicate-vector n i)
  (build-vector n (const i)))

(define (read-vector-from-csv fname)
  (call-with-input-file fname
    (lambda (in)
      (for/vector [(s (in-lines in))]
        (string->number s)))))
(define (write-vector-to-csv fname)
  (void))

(define (get-cmd-argument i)
  (vector-ref (current-command-line-arguments) i))
