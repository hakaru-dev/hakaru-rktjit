#lang racket

(require racket/splicing)

(require "ast.rkt"
         "sham-utils.rkt")

(provide gensym^
         symbol-append
         get-print-type
         map-ast
         real->prob
         prob->real
         logsumexp2
         logspace-add
         one-of-type
         read-vector-from-csv
         write-vector-to-csv
         replicate-vector
         zero-of-type)

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

;; func: ast -> ast, should return the same ast if no change
;; this map will call the func on each ast node
;; and then recursively call it again in on the output from the func
(define (map-ast func ast)
  (define (m body)
    (match (func body)
      [(expr-fun args ret-type body)
       (expr-fun args ret-type (m body))]
      [(expr-if t tst thn els)
       (expr-if t (m tst) (m thn) (m els))]
      [(expr-app t rt rds)
       (expr-app t (m rt) (map m rds))]
      [(expr-let type var val b)
       (expr-let type var (m val) (m b))]
      [(expr-sum t i start end b)
       (expr-sum t i (m start) (m end) (m b))]
      [(expr-prd t i start end b)
       (expr-prd t i (m start) (m end) (m b))]
      [(expr-arr t i end b)
       (expr-arr t i (m end) (m b))]
      [(expr-val t v)
       (expr-val t v)]
      [(expr-intr s)
       (expr-intr s)]
      [(expr-var t s o)
       (expr-var t s o)]))
  (m ast))


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
    [`(array ,tar) (string->symbol (format array-format (get-print-type tar)))]
    [`(measure ,t) (string->symbol (format measure-format (get-print-type t)))]
    [symbol? t]))
