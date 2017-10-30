#lang racket

(require (for-syntax racket/syntax))

(require ffi/unsafe)

(require sham/jit
         sham/ast
         "sham-utils.rkt")
(require (for-syntax "sham-utils.rkt"))
(provide basic-defines)

(define simple-funs
  (list
   (sham$define
    (nat2prob (v : nat) : prob)
    (return
     (sham$app real2prob (sham$app ui->fp (sham$var 'v) (sham:exp:type (sham:type:ref 'real))))))

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
    (add-3-real (v1 : real) (v2 : real) (v3 : real) : real)
    (return (sham$app fadd
                      (sham$app-var fadd v1 v2)
                      (sham$var 'v3))))

   (sham$define
    (add-2-prob (v1 : prob) (v2 : prob) : prob)
    (return (sham$app real2prob
                      (sham$app add-2-real
                                (sham$app-var prob2real v1)
                                (sham$app-var prob2real v2)))))

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
    (return (sham$app-var fadd v1 v2)))

   (sham$define
    (mul-4-prob (v1 : prob) (v2 : prob) (v3 : prob) (v4 : prob) : prob)
    (return (sham$app fadd
                      (sham$var 'v4)
                      (sham$app fadd (sham$var 'v3) (sham$app-var fadd v1 v2)))))))
;TODO make a sham util to have templated types

(define (pair-type t1 t2)
  (define pair-type-str (format pair-format t1 t2))
  (define pt-ref (sham:type:ref (string->symbol pair-type-str)))
  (define st1 (sham:type:ref t1))
  (define st2 (sham:type:ref t2))
  (list
   (sham:def:type (string->symbol pair-type-str)
                  (sham:type:struct (list pair-car-sym
                                          pair-cdr-sym)
                                    (list (sham:type:ref t1)
                                          (sham:type:ref t2))))
   (sham:def:type (string->symbol (string-append pair-type-str "*"))
                  (sham:type:pointer pt-ref))
   (sham:def:function
    (string->symbol (string-append "make-" pair-type-str))
    '() '(AlwaysInline)
    '(a b) (list st1 st2) (sham:type:pointer pt-ref)
    (sham:stmt:let
     '(pp ap bp)
     (list (sham:type:pointer pt-ref)
           (sham:type:pointer st1)
           (sham:type:pointer st2))
     (list (sham$app malloc (sham:exp:type pt-ref))
           (sham:exp:gep (sham$var 'pp) (list (nat-value 0) (nat-value 0)))
           (sham:exp:gep (sham$var 'pp) (list (nat-value 0) (nat-value 1)))))
    (sham$block
     (sham:stmt:exp (sham$app-var store! a ap))
     (sham:stmt:exp (sham$app-var store! b bp))
     (sham:stmt:return (sham$var 'pp))))))

(define probability-defs
  (list
   (sham:def:global 'gsl-rng (sham:type:ref 'void*))
   (sham:def:function
    'init-rng '() '() '() '() (sham:type:ref 'void)
    (sham:stmt:block
     (list
      (sham:stmt:set!
       (sham:exp:var 'gsl-rng)
       (sham:exp:app (sham:rator:external 'libgsl 'gsl_rng_alloc (sham:type:ref 'void*))
                     (list (sham:exp:external 'libgsl 'gsl_rng_taus (sham:type:ref 'void*)))))
      (sham:stmt:return-void))))
   (sham:def:function
    'uniform '() '()
    '(v1 v2) (list real real) real
    (sham:stmt:return
     (sham:exp:app (sham:rator:external 'libgsl 'gsl_ran_flat real)
                   (list (sham:exp:var 'gsl-rng) (sham:exp:var 'v1) (sham:exp:var 'v2)))))
   (sham:def:function
    'normal '() '()
    '(mean sigma) (list real prob) real
    (sham:stmt:return
     (sham:exp:app (sham:rator:symbol 'add-2-real)
                   (list
                    (sham:exp:var 'mean)
                    (sham:exp:app (sham:rator:external 'libgsl 'gsl_ran_gaussian real)
                                  (list (sham:exp:var 'gsl-rng)
                                        (sham:exp:app (sham:rator:symbol 'prob2real)
                                                      (list (sham:exp:var 'sigma)))))))))
   (sham:def:function
    'beta '() '()
    '(a b) (list prob prob) prob
    (sham:stmt:return
     (sham:exp:app (sham:rator:symbol 'real2prob)
                   (list
                    (sham:exp:app (sham:rator:external 'libgsl 'gsl_ran_beta real)
                                  (list (sham:exp:var 'gsl-rng)
                                        (sham:exp:app (sham:rator:symbol 'prob2real)
                                                      (list (sham:exp:var 'a)))
                                        (sham:exp:app (sham:rator:symbol 'prob2real)
                                                      (list (sham:exp:var 'b)))))))))
   (sham:def:function
    'gamma '() '()
    '(a b) (list prob prob) prob
    (sham:stmt:return
     (sham:exp:app (sham:rator:symbol 'real2prob)
                   (list
                    (sham:exp:app (sham:rator:external 'libgsl 'gsl_ran_gamma real)
                                  (list (sham:exp:var 'gsl-rng)
                                        (sham:exp:app (sham:rator:symbol 'prob2real)
                                                      (list (sham:exp:var 'a)))
                                        (sham:exp:app (sham:rator:symbol 'prob2real)
                                                      (list (sham:exp:var 'b)))))))))))
;; (sham:def:function
;;  'categorical '() '()
;;  '(a) (list (sham:type:ref 'array<prob>)) prob
;;  (sham:stmt:return 0))

(define simple-funs
  (list
   (sham$define
    (nat2prob (v : nat) : prob)
    (return
     (sham$app real2prob (sham$app ui->fp (sham$var 'v) (sham:exp:type (sham:type:ref 'real))))))

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
    (return (sham$app fmul (real-value -1.0) (sham$var 'v))))))

(define (add-basic-prelude prelude-defines)
  (map (λ (d) (hash-set! prelude-defines (sham:def-id d) d))
       (append simple-funs
               probability-defs)))

(define (get-size-ptr vsym)
  (sham:exp:gep (sham$var vsym) (list (nat-value 0) (nat-value 0))))
(define (get-data-ptr vsym)
  (sham:exp:gep (sham$var vsym) (list (nat-value 0) (nat-value 1))))

(define (get-array-data-type-def array-type-def)
  (match tdef
    [(sham:def:type _ (sham:type:struct _ (list _ t))) t]))



(define (add-array-defs-prelude tast tdef prelude)
  (define atref (get-sham-type-ref tdef))
  (define tpdefs (get-sham-type-define `(pointer ,tast)))
  (add-defs-prelude tpdefs prelude)
  (define tpdef (car tpdefs))
  (define apref (get-sham-type-ref tpdef))
  (define arr-id (sham:def-id tdef))
  (define array-data-type (get-array-data-type-def tdef))
  (define adt array-data-type)
  (define adt-ref (get-sham-type-ref array-data-type))
  (define adtp (sham:type:pointer adt-ref))
  
  (define (get-fun-name frmt)
    (string->symbol (format frmt arr-id)))
  
  (define defs
    (list
     (sham:def:function ;make-array
      (get-fun-name make-array-fun-format)
      '() '(AlwaysInline)
      array-args
      (list type-nat-ref adt-ref)
      apref
      (sham:stmt:let
       '(ap ap-size* ap-data*)
       (list a-type* type-nat-ref
             p-type*)
       (list (sham$app malloc (sham:exp:type atref))
             (get-size-ptr 'ap)
             (get-data-ptr 'ap))
       (sham$block
        (sham:stmt:exp (sham$app-var store! size ap-size*))
        (sham:stmt:exp (sham$app-var store! data ap-data*))
        (sham:stmt:return (sham$var 'ap)))))

     (sham:def:function ;get-array-data
      (get-fun-name get-array-data-fun-format)
      '() '(AlwaysInline)
      '(ap)
      (list tpdef) adt-ref
      (sham:stmt:let
       '(atp)
       (list adtp)
       (list (get-data-ptr 'ap))
       (sham:stmt:return (sham$app load (sham$var 'atp)))))

     (sham:def:function ;new-size-array
      (get-fun-name new-size-array-format)
      '() '(AlwaysInline)
      '(size)
      (list type-nat-ref) apref
      (sham:stmt:let
       '(ap data datap sizep)
       (list apref adt adtp type-nat-ref)
       (list (sham$app malloc (sham:exp:type atref))
             (sham$app arr-malloc (sham:exp:type adt-ref) (sham$var 'size))
             (get-data-ptr 'ap)
             (get-size-ptr 'ap))
       (sham$block
        (sham:stmt:exp (sham$app-var store! size sizep))
        (sham:stmt:exp (sham$app-var store! data datap))
        (sham:stmt:return (sham$var 'ap)))))

     (sham:def:function ;get-array-size
      (get-fun-name get-array-size-fun-format)
      '() '(AlwaysInline)
      '(array-ptr)
      (list apref) type-nat-ref
      (sham:stmt:return (sham$app load (get-size-ptr 'array-ptr))))


     (sham:def:function ;get-index
      (get-fun-name get-index-fun-format)
      '() '(AlwaysInline)
      '(array-ptr index)
      (list apref type-nat-ref) adt-ref
      (sham:stmt:return
       (sham$app load
                 (sham:exp:gep (sham$app load (get-data-ptr 'array-ptr))
                               (list (sham$var 'index))))))

     (sham:def:function ;set-index
      (get-fun-name set-index-fun-format)
      '() '(AlwaysInline)
      '(array-ptr index v)
      (list apref type-nat-ref adt-ref) (sham:type:ref 'void) 
      (sham$block
       (sham:stmt:exp-stmt
        (sham$app store! (sham$var 'v)
                  (sham:exp:gep (sham$app load (get-data-ptr 'array-ptr))
                                (list (sham$var 'index))))
        (sham:stmt:void))
       (sham:stmt:return-void)))

     (sham:def:function ;empty-array
      (get-fun-name empty-array-format)
      '() '(AlwaysInline)
      '()
      (list ) apref
      (sham:stmt:return
       (sham:exp:app (sham:rator:symbol (get-fun-name new-size-array-format))
                     (list (nat-value 0)))))))
  (add-defs-prelude defs prelude))

(define (add-pair-defs-prelude tast def prelude)
  ...)
(define (add-defs-for-type tast def prelude)
  (match tast
    [`(array ,tar) (add-array-defs-prelude tast def prelude)]
    [`(pair ,t1 ,t2) ...]
    [else (void)]))

(define (add-defs-prelude defs prelude)
  (map (λ (d) (hash-set! prelude (sham:def-id d) d)) defs))
;; (module+ test
;;   (require rackunit)
;;   (require "utils.rkt")
;;   (define mod (sham:module
;;                '((passes . ())
;;                  (ffi-libs . ((libgslcblas . ("libgslcblas" #:global? #t))
;;                               (libgsl . ("libgsl")))))
;;                (basic-defines)))
;;   (print-sham-ast mod)
;;   (define bmod (compile-module mod))
;;   ;  (jit-optimize-module bmod #:opt-level 3)
;;   (define benv (initialize-jit bmod))
;;   (jit-verify-module benv)
;;   (jit-dump-module benv)
  
;;   (define (get-t t) (jit-get-racket-type (env-lookup t benv)))
;;   (define (get-f f) (jit-get-function f benv))
  
;;   (define t-real (get-t 'real))
;;   (define t-nat (get-t 'nat))
;;   (define t-prob (get-t 'prob))

;;   (define c-nat2prob (get-f 'nat2prob))
;;   (define c-prob2real (get-f 'prob2real))
;;   (define c-real2prob (get-f 'real2prob))

;;   (define recip-nat (get-f 'recip-nat))
;;   (define recip-real (get-f 'recip-real))
;;   (define recip-prob (get-f 'recip-prob))

;;   (define add-2-nat (get-f 'add-2-nat))
;;   (define add-2-real (get-f 'add-2-real))
;;   (define add-3-real (get-f 'add-3-real))
;;   (define add-2-prob (get-f 'add-2-prob))
;;   (define add-3-prob (get-f 'add-3-prob))
  
;;   (define mul-2-nat (get-f 'mul-2-nat))
;;   (define mul-2-real (get-f 'mul-2-real))
;;   (define mul-2-prob (get-f 'mul-2-prob))
;;   (define mul-4-prob (get-f 'mul-4-prob))

;;   (define e 0.00000000001)
;;   (check-= (c-nat2prob 8) (c-real2prob 8.0) e)
;;   (check-= (c-real2prob 1.2345) (real->prob 1.2345) e)
;;   (check-= (c-prob2real 1.2345) (prob->real 1.2345) e)

;;   (check-= (recip-nat 2) 0.5 e)
;;   (check-= (recip-real 5.2345) (/ 1.0 5.2345) e)
;;   (check-= (recip-prob (c-real2prob 5.2345)) (real->prob (/ 1.0 5.2345)) e)
;;   (check-= (recip-prob 14.124515) (real->prob (/ 1.0 (prob->real 14.124515))) e)
  
;;   (check-eq? (add-2-nat 3 4) 7)
;;   (check-= (add-2-real 1.234 543.1234) (+ 1.234 543.1234) e)
;;   (check-= (add-3-real 5.324 543.2432 89.43241) (+ 5.324 543.2432 89.43241) e)
;;   (check-= (add-2-prob 1.234 543.1234)
;;            (real->prob (+ (prob->real 1.234) (prob->real 543.1234)))
;;            e)
;;   (check-= (add-3-prob 5.324 543.2432 89.43241)
;;            (logspace-add 5.324 543.2432 89.43241)
;;            e)
;;   (check-eq? (mul-2-nat 4 5) 20)
;;   (check-= (mul-2-real 4.123 5.3123) (* 4.123 5.3123) e)
;;   (check-= (mul-2-prob 4.123 5.3123) (+ 4.123 5.3123) e)
;;   (check-= (mul-2-prob 4.123 5.3123)
;;            (real->prob (* (prob->real 4.123) (prob->real 5.3123)))
;;            e)

;;   (define ta '(1.0 2.0 3.0 3.14 42.23))
;;   (define ti '(1 2 3 4 42))
;;   (define test-nat-array (list->cblock ti t-nat))
;;   (define test-real-array (list->cblock ta t-real))
;;   (define test-prob-array (list->cblock (map real->prob ta) t-prob))

;;   (define make-array-nat (get-f 'make-array<nat>))
;;   (define get-ptr-array-nat (get-f 'get-ptr-array<nat>))
;;   (define empty-array-nat (get-f 'empty-array<nat>))
;;   (define size-nat (get-f 'size-array<nat>))
;;   (define index-array-nat (get-f 'index-array<nat>))
;;   (define set-array-nat-at-index! (get-f 'set-index-in-array<nat>))
;;   (define empty-zero-array-nat (get-f 'empty-zero-array<nat>))

;;   (define tiarr (make-array-nat (length ti) test-nat-array))
;;   (define eti (empty-array-nat 5))
;;   (check-eq? (size-nat tiarr) 5)
;;   (check-eq? (index-array-nat tiarr 4) 42)
;;   (set-array-nat-at-index! tiarr 3 23)
;;   (check-eq? (index-array-nat tiarr 3) 23)

;;   (check-eq? (size-nat eti) 5)
;;   (set-array-nat-at-index! eti 3 42)
;;   (check-eq? (index-array-nat eti 3) 42)

;;   (define make-array-real (get-f 'make-array<real>))
;;   (define empty-array-real (get-f 'empty-array<real>))
;;   (define index-array-real (get-f 'index-array<real>))
;;   (define size-real (get-f 'size-array<real>))
;;   (define set-array-real-at-index! (get-f 'set-index-in-array<real>))


;;   (define trarr (make-array-real (length ti) test-real-array))
;;   (define etr (empty-array-real 5))
;;   (check-eq? (size-real trarr) 5)
;;   (check-= (index-array-real trarr 4) 42.23 e)
;;   (set-array-real-at-index! trarr 3 23.42)
;;   (check-= (index-array-real trarr 3) 23.42 e)

;;   (check-eq? (size-real etr) 5)
;;   (set-array-real-at-index! etr 3 42.23)
;;   (check-= (index-array-real etr 3) 42.23 e)

;;   (define make-array-prob (get-f 'make-array<prob>))
;;   (define index-array-prob (get-f 'index-array<prob>))
;;   (define size-prob (get-f 'size-array<prob>))
;;   (define set-array-prob-at-index! (get-f 'set-index-in-array<prob>))
;;   (define empty-array-prob (get-f 'empty-array<prob>))  

;;   (define tparr (make-array-prob (length ti) test-prob-array))
;;   (define etp (empty-array-prob 5))
;;   (check-eq? (size-prob tparr) 5)
;;   (check-= (index-array-prob tparr 4) (real->prob 42.23) e)
;;   (set-array-prob-at-index! tparr 3 23.42)
;;   (check-= (index-array-prob tparr 3) 23.42 e)

;;   (check-eq? (size-prob etp) 5)
;;   (set-array-prob-at-index! etp 3 42.23)
;;   (check-= (index-array-prob etp 3) 42.23 e)

;;   (define make-array-array-nat (get-f 'make-array<array<nat>*>))
;;   (define empty-array-array-nat (get-f 'empty-array<array<nat>*>))
;;   (define index-array-array-nat (get-f 'index-array<array<nat>*>))
;;   (define size-array-array-nat (get-f 'size-array<array<nat>*>))
;;   (define set-array-array-nat-at-index! (get-f 'set-index-in-array<array<nat>*>))


;;   (define etai (empty-array-array-nat 5))
;;   (set-array-array-nat-at-index! etai 2 eti)
;;   (define etib (index-array-array-nat etai 2))
;;   (check ptr-equal? etib eti)
;;   (check-eq? (index-array-nat etib 3) 42)
;;   (define init-rng (get-f 'init-rng))
;;   (init-rng)
;;   (define uniform (get-f 'uniform))
;;   (define normal (get-f 'normal))

;;   (printf "random normal mu=0, sd=1: ~a\n" (normal 0.0 (c-real2prob 1.0)))
;;   (printf "random uniform 1-5: ~a\n" (uniform 1.0 500.0)))
  
