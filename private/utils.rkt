#lang racket

(require racket/splicing)

(require "ast.rkt"
         "sham-utils.rkt")

(provide (all-defined-out))

(define debug-pass (make-parameter #f))
(define-syntax-rule (debug-printf args ...)
  (when (debug-pass)
    (printf args ...)))

(splicing-let ([gensym-hash (make-hash)])
  (define (gensym^ sym [sep ""])
    (define n (add1 (hash-ref gensym-hash sym 0)))
    (hash-set! gensym-hash sym n)
    (string->symbol (string-append (symbol->string sym)
                                   sep
                                   (number->string n)))))
(define (symbol-append s1 s2)
  (string->symbol (string-append (symbol->string s1)
				 (symbol->string s2))))

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

(define (get-print-type t)
  (match t
    [`(* ,tp) (string->symbol (format pointer-format (get-print-type tp)))]
    [`(array ,tar) (string->symbol (format array-format (if (pair? tar)
                                                            (get-print-type `(* ,tar))
                                                            (get-print-type tar))))]
    [`(measure ,t) (get-print-type t)]
    [`(pair ,t1 ,t2) (string->symbol (format pair-format
                                             (get-print-type
                                              (if (pair? t1)
                                                  `(* ,t1)
                                                  t1))
                                             (get-print-type
                                              (if (pair? t2)
                                                  `(* ,t2)
                                                  t2))))]
    [symbol? t]))
