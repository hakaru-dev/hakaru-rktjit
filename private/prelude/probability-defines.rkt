#lang racket

(require sham
         (submod sham/ast utils))
(require "template-format.rkt"
         "type-defines.rkt"
         "basic-defines.rkt"
         "array-defines.rkt"
         "utils.rkt")

(provide probability-defs
         probability-rator?
         get-probability-rator)

(define (probability-rator? sym)
  (member sym '(uniform normal beta gamma
                        betafunc realbetafunc betafuncreal gammafunc
                        categorical
                        superpose-categorical superpose-categorical-real)))

(define (get-probability-rator sym tresult trands)
  (cond [(equal? sym 'superpose-categorical) (build-superpose-categorical tresult (length trands))]
        [(equal? sym 'superpose-categorical-real) (build-superpose-categorical-real tresult (length trands))]
        [else (values (sham:rator:symbol sym) (void))]))

(define (probability-defs)
  (define adefs (append
                 (array-defs `(array nat))
                 (array-defs `(array real))
                 (array-defs `(array prob))))
  (define tvoid (sham:type:ref 'void))
  (define tvoid* (sham:type:ref 'void*))
  (define tnat (sham:type:ref 'nat))
  (define treal (sham:type:ref 'real))
  (define tprob (sham:type:ref 'prob))
  (append
   adefs
   (list
    (sham:def:global (void) 'gsl-rng tvoid*)

    (sham$define
     (init-rng (prelude-function-info) tvoid)
     (sham:stmt:block
      (list
       (sham:stmt:set!
        (sham:expr:var 'gsl-rng)
        (sham:expr:app (sham:rator:external 'libgsl 'gsl_rng_alloc tvoid*)
                       (list (sham:expr:external 'libgsl 'gsl_rng_taus tvoid*))))
       (sham:stmt:return (sham:expr:void)))))

    (sham$define
     (uniform (prelude-function-info) (v1 treal) (v2 treal) treal)
     (sham:stmt:return
      (sham:expr:app (sham:rator:external 'libgsl 'gsl_ran_flat treal)
                     (list (sham:expr:var 'gsl-rng)
                           (sham$var 'v1) (sham$var 'v2)))))

    (sham$define
     (normal (prelude-function-info) (mean treal) (sigma tprob) treal)
     (sham:stmt:return
      (sham:expr:app (sham:rator:symbol 'fadd)
                     (list
                      (sham$var 'mean)
                      (sham:expr:app
                       (sham:rator:external 'libgsl 'gsl_ran_gaussian treal)
                       (list (sham$var 'gsl-rng)
                             (sham:expr:app (sham:rator:symbol 'prob2real)
                                            (list (sham$var 'sigma)))))))))

    (sham$define
     (beta (prelude-function-info) (a tprob) (b tprob) tprob)
     ;; (sham:stmt:expr (sham$app print-prob a))
     ;; (sham:stmt:expr (sham$app print-prob b))
     (sham:stmt:return
      (sham:expr:app (sham:rator:symbol 'betafuncreal)
                     (list (sham:expr:app (sham:rator:symbol 'prob2real)
                                          (list (sham$var 'a)))
                           (sham:expr:app (sham:rator:symbol 'prob2real)
                                          (list (sham$var 'b)))))))


    (sham$define
     (realbetafunc (prelude-function-info) (a treal) (b treal) treal)
     (sham:stmt:return
      (sham:expr:app (sham:rator:symbol 'prob2real)
                     (list (sham:expr:app (sham:rator:symbol 'betafuncreal) (list (sham$var a) (sham$var  b)))))))

    (sham$define
     (betafuncreal (prelude-function-info) (a treal) (b treal) tprob)
     (sham:stmt:return (sham:expr:app (sham:rator:external 'libgsl 'gsl_sf_lnbeta treal)
                                      (list (sham$var a) (sham$var  b)))))

    (sham$define
     (betafunc (prelude-function-info) (a tprob) (b tprob) tprob)
     (sham:stmt:return
      (sham:expr:app (sham$rator betafuncreal)
                     (list (sham$app prob2real a) (sham$app prob2real b)))))

    (sham$define
     (gamma (prelude-function-info) (a tprob) (b tprob) tprob)
     (sham:stmt:return
      (sham:expr:app (sham:rator:symbol 'real2prob)
                     (list
                      (sham:expr:app (sham:rator:external 'libgsl 'gsl_ran_gamma treal)
                                     (list (sham:expr:var 'gsl-rng)
                                           (sham:expr:app (sham:rator:symbol 'prob2real)
                                                          (list (sham$var 'a)))
                                           (sham:expr:app (sham:rator:symbol 'prob2real)
                                                          (list (sham$var 'b)))))))))
    (sham$define
     (gammaFunc (prelude-function-info) (a tprob) tprob)
     (sham:stmt:return
      (sham:expr:app (sham:rator:symbol 'real2prob)
                     (list
                      (sham:expr:app (sham:rator:external 'libgsl 'gsl_sf_gamma treal)
                                     (list
                                      (sham:expr:app (sham:rator:symbol 'prob2real)
                                                     (list (sham$var 'a)))))))))

    (sham:def:function
     (prelude-function-info)
     'print-prob (list 'a) (list (sham:type:ref 'f64)) (sham:type:ref 'void)
     (sham:stmt:block
      (list
       (sham:stmt:expr (sham:expr:app (sham:rator:racket 'rktprint
                                                         (λ (a) (printf "printing: ~a\n" a))
                                                         (sham:type:function (list (sham:type:ref 'f64))
                                                                             (sham:type:ref 'void)))
                                      (list (sham:expr:var 'a))))
       (sham:stmt:return (sham:expr:void)))))
    (sham$define
     (categorical-real (prelude-function-info) (arr (sham:type:ref 'array<real>*)) tnat)
     (sham:stmt:return (sham:expr:app (gsl-rator 'gsl_ran_discrete tnat)
                                      (list (sham$var gsl-rng)
                                            (sham:expr:app (gsl-rator 'gsl_ran_discrete_preproc tvoid*)
                                                           (list (sham$app get-size$array<real> arr)
                                                                 (sham$app get-data$array<real> arr)))))))
    (sham$define ;;TODO implement categorical using gsl_ran_discrete
     (categorical (prelude-function-info) (arp (sham:type:ref 'array<prob>*)) tnat)
     (sham:stmt:expr
      (sham:expr:let
       '(arr i)
       (list (sham:type:ref 'array<real>*)
             (sham:type:ref 'nat))
       (list (sham$app new-sized$array<real>
                       (sham$app get-size$array<prob> arp))
             (nat-value 0))
       (sham:stmt:block
        (list (sham:stmt:while
               (sham$app icmp-eq i (sham$app get-size$array<prob> arp))
               (sham:stmt:expr (sham$app set-index!$array<real> arr i
                                         (sham$app prob2real
                                                   (sham$app get-index$array<prob> arp i)))))
              (sham$return (sham$app categorical-real arr))))
       (nat-value 0)))))))


(define (gsl-rator sym type)
  (sham:rator:external 'libgsl sym type))

(define (build-superpose-categorical tresult len) ;;TODO we can probably optimize len two with binomial
  (define len 2)
  (define fun-name (string->symbol (format "categorical-~a" len)))
  (define func
    (sham:def:function
     (prelude-function-info) fun-name
     (build-list 2 get-vi) (build-list 2 (const (sham:type:ref 'prob)))
     (sham:type:ref tresult)
     (sham:stmt:if (sham$app fcmp-ule (get-vi 0) (get-vi 1))
                   (sham:stmt:return (nat-value 1))
                   (sham:stmt:return (nat-value 0)))))
  ;;TODO free arr :P
  (values (sham:rator:symbol fun-name) func))

(define (build-superpose-categorical-real tresult len) ;;TODO we can probably optimize len two with binomial
  (define len 2)
  (define fun-name (string->symbol (format "categorical-~a" len)))
  (define func
    (sham:def:function
     (prelude-function-info) fun-name
     (build-list 2 get-vi) (build-list 2 (const (sham:type:ref 'real)))
     (sham:type:ref tresult)
     (sham:stmt:if (sham$app fcmp-ule (get-vi 0) (get-vi 1))
                   (sham:stmt:return (nat-value 1))
                   (sham:stmt:return (nat-value 0)))))
  ;;TODO free arr :P
  (values (sham:rator:symbol fun-name) func))

;; (define (build-superpose-categorical tresult len) ;;TODO we can probably optimize len two with binomial
;;   (define fun-name (string->symbol (format "categorical-~a" len)))
;;   (define func
;;     (sham:def:function
;;      (prelude-function-info) fun-name
;;      (build-list len get-vi) (build-list len (const (sham:type:ref 'prob)))
;;      (sham:type:ref tresult)
;;      (sham:stmt:expr
;;       (sham:expr:let (list 'arr)
;;                      (list (sham:type:ref 'array<prob>*))
;;                      (list (sham:expr:app
;;                             (sham:rator:symbol
;;                              (string->symbol (format new-size-array-fun-format
;;                                                      'array<prob>)))
;;                             (list (nat-value len))))
;;                      (sham:stmt:block
;;                       (append
;;                        (for/list [(i (in-range len))]
;;                          (sham:stmt:expr
;;                           (sham:expr:app (sham:rator:symbol 'print-prob)
;;                                          (list (sham$var (get-vi i))))))
;;                        (for/list [(i (in-range len))]
;;                          (sham:stmt:expr
;;                           (sham:expr:app (sham:rator:symbol
;;                                           (string->symbol (format set-index-fun-format
;;                                                                   'array<prob>)))
;;                                          (list (sham$var 'arr)
;;                                                (nat-value i)
;;                                                (sham$var (get-vi i))))))

;;                        (list
;;                         (sham:stmt:return
;;                          (if (equal? tresult 'nat)
;;                              (sham:expr:app (sham:rator:symbol 'categorical)
;;                                             (list (sham$var 'arr)))
;;                              (sham:expr:app (sham:rator:symbol 'intcast)
;;                                             (list
;;                                              (sham:expr:app (sham:rator:symbol 'categorical)
;;                                                             (list (sham$var 'arr)))
;;                                              (sham:expr:type (sham:type:ref tresult)))))))))

;;                      (sham:expr:void))))) ;;TODO free arr :P
;;   (values (sham:rator:symbol fun-name) func))


(module+ test
  (require rackunit
           sham/jit
           ffi/unsafe
           "basic-defines.rkt")


  (define-values (_ sc2) (build-superpose-categorical 'bool 2))
  (define defs (append (basic-defs)
                       (probability-defs)
                       (list sc2)))



  ;(pretty-print (map sham-def->sexp defs))

  (define mod
    (sham:module (basic-mod-info) defs))
  (define cmod (compile-module mod))

  (optimize-module cmod)
  ;(jit-dump-module cmod)
  (jit-verify-module cmod)
  (initialize-jit! cmod)
  (pretty-print cmod)
  (define (get-t t) (jit-get-racket-type t cmod))
  (define (get-f f) (jit-get-function f cmod))

  (define t-real (get-t 'real))
  (define t-nat (get-t 'nat))
  (define t-prob (get-t 'prob))

  (define c-nat2prob (get-f 'nat2prob))
  (define c-prob2real (get-f 'prob2real))
  (define c-real2prob (get-f 'real2prob))

  (define init-rng (get-f 'init-rng))
  (init-rng)
  (define uniform (get-f 'uniform))
  (define normal (get-f 'normal))
  (define beta (get-f 'beta))
  (define categorical-2 (get-f 'categorical-2))
  (define categorical-real (get-f 'categorical-real))

  (define betaFunc (get-f 'betafunc))
  (define betaFuncreal (get-f 'betafuncreal))
  (define realbetaFunc (get-f 'realbetafunc))
  (define ars '(array real))
  (define ((gf frmt) tsym)
    (get-f (get-fun-symbol frmt (get-type-string tsym))))
  (define make-f (gf make-array-fun-format))
  (define new-size-f (gf new-size-array-fun-format))
  (define empty-f (gf empty-array-fun-format))
  (define size-f (gf get-array-size-fun-format))
  (define data-f (gf get-array-data-fun-format))
  (define index-f (gf get-index-fun-format))
  (define index!-f (gf set-index-fun-format))

  (define make-array-real (make-f ars))
  (define new-sized-array-real (new-size-f ars))
  (define empty-array-real (empty-f ars))
  (define get-data-array-real (data-f ars))
  (define get-size-array-real (size-f ars))
  (define get-index-array-real (index-f ars))
  (define set-index-array-real (index!-f ars))

  ;  (define t-real (get-t 'real))
  (define ta '(0.0 0.0 4.0))
  (define test-real-array (list->cblock ta t-real))
  (define test-arr (make-array-real (length ta) test-real-array))

  (betaFunc (c-real2prob 4.0) (c-real2prob 4.0))
  ;; hkp logFromlogFloat $ betaFunc (prob_ 4.0) (prob_ 4.0)
  (printf "cat-real : ~a\n" (categorical-real test-arr))
  (printf "cat2: ~a\n" (categorical-2 100.0 23.0))
  (printf "random normal mu=0, sd=1: ~a\n" (normal 0.0 (c-real2prob 1.0)))
  (printf "random uniform 1-5: ~a\n" (uniform 1.0 5.0)))
