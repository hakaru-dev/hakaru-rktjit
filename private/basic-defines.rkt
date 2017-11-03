#lang racket

(require (for-syntax racket/syntax))

(require ffi/unsafe)

(require sham/jit
         sham/ast
         "sham-utils.rkt"
         "type-defines.rkt")

(provide (all-defined-out))

(define (basic-defs)
  (define nat type-nat-ref)
  (define nat* (sham:type:ref 'nat*))
  (define real type-real-ref)
  (define prob type-prob-ref)

  (list
   type-nat-def
   type-real-def
   type-prob-def
   (sham:def:type 'nat* (sham:type:pointer type-nat-ref))
   (sham:def:type 'real* (sham:type:pointer type-real-ref))
   (sham:def:type 'prob* (sham:type:pointer type-prob-ref))

   (sham$define
    (nat2prob (v : nat) : prob)
    (return
     (sham$app real2prob
               (sham$app ui->fp
                         (sham$var 'v) (sham:exp:type (sham:type:ref 'real))))))

   (sham$define
    (nat2real (v : nat) : real)
    (return
     (sham$app ui->fp (sham$var 'v) (sham:exp:type (sham:type:ref 'real)))))

   (sham$define
    (prob2real (v : prob) : real)
    (return (sham$app-var (llvm.exp.f64 prob) v)))

   (sham$define
    (real2prob (v : real) : prob)
    (return (sham$app-var (llvm.log.f64 prob) v)))

   (sham$define
    (recip-nat (v : nat) : real)
    (return (sham$app fdiv (real-value 1.0)
                      (sham$app ui->fp (sham$var 'v)
                                (sham:exp:type (sham:type:ref 'real))))))
   (sham$define
    (recip-real (v : real) : real)
    (return (sham$app fdiv (real-value 1.0) (sham$var 'v))))

   (sham$define
    (recip-prob (v : real) : real)
    (return (sham$app fmul (real-value -1.0) (sham$var 'v))))

   (sham$define
    (add-2-nat (v1 : nat) (v2 : nat) : nat)
    (return (sham$app-var add-nuw v1 v2)))
   (sham$define
    (add-2-real (v1 : real) (v2 : real) : real)
    (return (sham$app-var fadd v1 v2)))
   (sham$define
    (add-2-prob (v1 : prob) (v2 : prob) : prob)
    (return (sham$app real2prob
                      (sham$app add-2-real
                                (sham$app-var prob2real v1)
                                (sham$app-var prob2real v2)))))

   (sham$define
    (add-3-real (v1 : real) (v2 : real) (v3 : real) : real)
    (return (sham$app fadd
                      (sham$app-var fadd v1 v2)
                      (sham$var 'v3))))
   (sham$define
    (add-3-prob (v1 : prob) (v2 : prob) (v3 : prob) : prob)
    (return (sham$app real2prob
                      (sham$app add-3-real
                                (sham$app-var prob2real v1)
                                (sham$app-var prob2real v2)
                                (sham$app-var prob2real v3)))))

   (sham$define
    (mul-2-nat (v1 : nat) (v2 : nat) : nat)
    (return (sham$app-var mul-nuw v1 v2)))
   (sham$define
    (mul-2-real (v1 : real) (v2 : real) : real)
    (return (sham$app-var fmul v1 v2)))
   (sham$define
    (mul-2-prob (v1 : prob) (v2 : prob) : prob)
    (return (sham$app-var fadd v1 v2)))))

(define (simple-rator? sym)
  (member sym
          '(nat2prob nat2real prob2real real2prob
                     recip-nat recip-real recip-prob
                     add-2-nat add-2-real add-2-prob
                     add-3-real add-3-prob
                     mul-2-nat mul-2-real mul-2-prob
                     uniform normal beta gamma categorical)))
(define math-rator?
  (curryr member '(+ * < > / == -)))
(define (get-math-rator sym tresult trands)
  (values '() (sham:rator:symbol 'math)))

(module+ test
  (require rackunit)
  (require "utils.rkt")
  (define mod (sham:module
               '((passes . ())
                 (ffi-libs . ()))
               (basic-defs)))
  ;(pretty-print (sham-ast->sexp mod))
  (define bmod (compile-module mod))

  (jit-optimize-module bmod #:opt-level 3)
  (define benv (initialize-jit bmod))
  (jit-dump-module benv)

  (define (get-t t) (jit-get-racket-type (env-lookup t benv)))
  (define (get-f f) (jit-get-function f benv))

  (define t-real (get-t 'real))
  (define t-nat (get-t 'nat))
  (define t-prob (get-t 'prob))

  (define c-nat2prob (get-f 'nat2prob))
  (define c-prob2real (get-f 'prob2real))
  (define c-real2prob (get-f 'real2prob))

  (define recip-nat (get-f 'recip-nat))
  (define recip-real (get-f 'recip-real))
  (define recip-prob (get-f 'recip-prob))

  (define add-2-nat (get-f 'add-2-nat))
  (define add-2-real (get-f 'add-2-real))
  (define add-3-real (get-f 'add-3-real))
  (define add-2-prob (get-f 'add-2-prob))
  (define add-3-prob (get-f 'add-3-prob))

  (define mul-2-nat (get-f 'mul-2-nat))
  (define mul-2-real (get-f 'mul-2-real))
  (define mul-2-prob (get-f 'mul-2-prob))

  (define e 0.00000000001)
  (check-= (c-nat2prob 8) (c-real2prob 8.0) e)
  (check-= (c-real2prob 1.2345) (real->prob 1.2345) e)
  (check-= (c-prob2real 1.2345) (prob->real 1.2345) e)

  (check-= (recip-nat 2) 0.5 e)
  (check-= (recip-real 5.2345) (/ 1.0 5.2345) e)
  (check-= (recip-prob (c-real2prob 5.2345)) (real->prob (/ 1.0 5.2345)) e)
  (check-= (recip-prob 14.124515) (real->prob (/ 1.0 (prob->real 14.124515))) e)

  (check-eq? (add-2-nat 3 4) 7)
  (check-= (add-2-real 1.234 543.1234) (+ 1.234 543.1234) e)
  (check-= (add-3-real 5.324 543.2432 89.43241) (+ 5.324 543.2432 89.43241) e)
  (check-= (add-2-prob 1.234 543.1234)
           (real->prob (+ (prob->real 1.234) (prob->real 543.1234)))
           e)
  (check-= (add-3-prob 5.324 543.2432 89.43241)
           (logspace-add 5.324 543.2432 89.43241)
           e)
  (check-eq? (mul-2-nat 4 5) 20)
  (check-= (mul-2-real 4.123 5.3123) (* 4.123 5.3123) e)
  (check-= (mul-2-prob 4.123 5.3123) (+ 4.123 5.3123) e)
  (check-= (mul-2-prob 4.123 5.3123)
           (real->prob (* (prob->real 4.123) (prob->real 5.3123)))
           e))
