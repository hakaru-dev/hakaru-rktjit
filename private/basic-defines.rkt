#lang racket

(require (for-syntax racket/syntax))

(require ffi/unsafe)

(require sham/jit
         sham/ast
         "sham-utils.rkt")

(provide (all-defined-out))

(define nat type-nat-ref)
(define nat* (sham:type:pointer nat))
(define real type-real-ref)
(define prob type-prob-ref)

(define probability-defs
  (list
   (sham:def:global 'gsl-rng (sham:type:ref 'void*))
   (sham:def:function
    'init-rng '() '() '() '() (sham:type:ref 'void)
    (sham:stmt:block
     (list
      (sham:stmt:set!
       (sham:exp:var 'gsl-rng)
       (sham:exp:app (sham:rator:external 'libgsl
                                          'gsl_rng_alloc
                                          (sham:type:ref 'void*))
                     (list (sham:exp:external 'libgsl 'gsl_rng_taus
                                              (sham:type:ref 'void*)))))
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
                                                      (list (sham:exp:var 'b)))))))))
   (sham:def:function ;;TODO test categorical
    'categorical '() '()
    '(a) (list (sham:type:ref 'array<prob>)) (sham:type:pointer nat)
    (sham:stmt:let
     '(n)
     (list (sham:type:ref 'array<nat>))
     (list (sham$app malloc (sham:exp:type (sham:type:ref 'array<nat>))))
     (sham:stmt:return
      (sham:exp:stmt-exp
       (sham:stmt:exp
        (sham:exp:app
         (sham:rator:external 'libgsl 'gsl_ran_multinomial (sham:type:pointer nat))
         (list (sham:exp:var 'gsl-rng)
               (sham:exp:app
                (sham:rator:symbol (string->symbol
                                    (format get-array-size-fun-format
                                            'array<prob>)))
                (sham$var 'a))
               (nat-value 1)
               (sham:exp:app
                (sham:rator:symbol (string->symbol
                                    (format get-array-data-fun-format
                                            'array<prob>)))
                (sham$var 'a))
               (sham$var 'n))))

       (sham$var 'n)))))))

(define simple-funs
  (list
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
    (return (sham$app fmul (real-value -1.0) (sham$var 'v))))))

(define (add-basic-prelude prelude-defines)
  (add-defs-prelude simple-funs)
  (add-defs-prelude probability-defs))

(define (add-array-defs-prelude tdef prelude)
  (add-defs-prelude (array-defs tdef) prelude))

(define (pair-defs tdef)
  (match-define (sham:type:struct _ (list at bt)) tdef)
  (define tr (get-sham-type-ref tdef))
  (define tp (sham:type:pointer tr))
  (define atref (get-sham-type-ref at))
  (define btref (get-sham-type-ref bt))
  (define atp (sham:type:pointer at))
  (define btp (sham:type:pointer bt))
  (define (get-fun-name frmt)
    (string->symbol (format frmt (sham:def-id tdef))))
  (list
   (sham:def:function
    (get-fun-name make-pair-fun-format) ;;make-pair
    '() '(AlwaysInline)
    '(a b) (list atref btref) tr
    (sham:stmt:let
     '(pp ap bp)
     (list tp atp btp)
     (list (sham$app malloc (sham:exp:type tr))
           (sham:exp:gep (sham$var 'pp) (list (nat-value 0) (nat-value 0)))
           (sham:exp:gep (sham$var 'pp) (list (nat-value 0) (nat-value 1)))))
    (sham$block
     (sham:stmt:exp (sham$app-var store! a ap))
     (sham:stmt:exp (sham$app-var store! b bp))
     (sham:stmt:return (sham$var 'pp))))
   
   (sham:def:function
    (get-fun-name pair-car-fun-format) ;;car
    '() '(AlwaysInline)
    '(p) (list tp) at
    (sham:stmt:return (sham$app load (get-size-ptr 'p))))
   ;;TODO hack as the field number are same for array and pair
   
   (sham:def:function
    (get-fun-name pair-cdr-fun-format)
    '() '(AlwaysInline)
    '(p) (list tp) bt
    (sham:stmt:return (sham$app load (get-data-ptr 'p))))))

(define (add-pair-defs-prelude tdef prelude)
  (add-defs-prelude (pair-defs tdef) prelude))

(define (add-defs-for-type tast def prelude)
  (match tast
    [`(array ,tar) (add-array-defs-prelude tast def prelude)]
    [`(pair ,t1 ,t2) (add-pair-defs-prelude tast def prelude)]
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
  
