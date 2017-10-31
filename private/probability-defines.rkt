#lang racket

(require sham/ast)
(require "template-format.rkt"
         "type-defines.rkt"
         "array-defines.rkt"
         "basic-defines.rkt"
         "prelude.rkt")
(provide probability-defs)

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
    (sham:def:global 'gsl-rng tvoid*)

    (sham:def:function
     'init-rng '() '() '() '() tvoid
     (sham:stmt:block
      (list
       (sham:stmt:set!
        (sham:exp:var 'gsl-rng)
        (sham:exp:app (sham:rator:external 'libgsl 'gsl_rng_alloc tvoid*)
                      (list (sham:exp:external 'libgsl 'gsl_rng_taus tvoid*))))
       (sham:stmt:return-void))))

    (sham:def:function
     'uniform '() '()
     '(v1 v2) (list treal treal) treal
     (sham:stmt:return
      (sham:exp:app (sham:rator:external 'libgsl 'gsl_ran_flat treal)
                    (list (sham:exp:var 'gsl-rng)
                          (sham$var 'v1) (sham$var 'v2)))))

    (sham:def:function
     'normal '() '()
     '(mean sigma) (list treal tprob) treal
     (sham:stmt:return
      (sham:exp:app (sham:rator:symbol 'add-2-real)
                    (list
                     (sham$var 'mean)
                     (sham:exp:app
                      (sham:rator:external 'libgsl 'gsl_ran_gaussian treal)
                      (list (sham$var 'gsl-rng)
                            (sham:exp:app (sham:rator:symbol 'prob2real)
                                          (list (sham$var 'sigma)))))))))
    (sham:def:function
     'beta '() '()
     '(a b) (list tprob tprob) tprob
     (sham:stmt:return
      (sham:exp:app (sham:rator:symbol 'real2prob)
                    (list
                     (sham:exp:app (sham:rator:external 'libgsl 'gsl_ran_beta treal)
                                   (list (sham:exp:var 'gsl-rng)
                                         (sham:exp:app (sham:rator:symbol 'prob2real)
                                                       (list (sham$var 'a)))
                                         (sham:exp:app (sham:rator:symbol 'prob2real)
                                                       (list (sham$var 'b)))))))))
    (sham:def:function
     'gamma '() '()
     '(a b) (list tprob tprob) tprob
     (sham:stmt:return
      (sham:exp:app (sham:rator:symbol 'real2prob)
                    (list
                     (sham:exp:app (sham:rator:external 'libgsl 'gsl_ran_gamma treal)
                                   (list (sham:exp:var 'gsl-rng)
                                         (sham:exp:app (sham:rator:symbol 'prob2real)
                                                       (list (sham$var 'a)))
                                         (sham:exp:app (sham:rator:symbol 'prob2real)
                                                       (list (sham$var 'b)))))))))

    (sham:def:function ;;TODO test categorical
     'categorical '() '()
     '(a) (list (sham:type:ref 'array<prob>)) (sham:type:pointer tnat)
     (sham:stmt:let
      '(n)
      (list (sham:type:ref 'array<nat>))
      (list (sham$app malloc (sham:exp:type (sham:type:ref 'array<nat>))))
      (sham:stmt:return
       (sham:exp:stmt-exp
        (sham:stmt:exp
         (sham:exp:app
          (sham:rator:external 'libgsl 'gsl_ran_multinomial (sham:type:pointer tnat))
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

        (sham$var 'n))))))))

(module+ test
  (require rackunit
           sham/jit
           ffi/unsafe
           "utils.rkt")

  (define defs (append (basic-defs)
                       (probability-defs)))
    
  ;(pretty-print (map sham-def->sexp defs))
  (define mod
    (sham:module
     '((passes . ())
       (ffi-libs . ((libgslcblas . ("libgslcblas" #:global? #t))
                    (libgsl . ("libgsl")))))
     defs))
  (define cmod (compile-module mod))

  (jit-optimize-module cmod #:opt-level 3)
  ;(jit-dump-module cmod)
  (define cjmod (initialize-jit cmod))
  ;(pretty-print cmod)
  (define (get-t t) (jit-get-racket-type (env-lookup t cmod)))
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

  (printf "random normal mu=0, sd=1: ~a\n" (normal 0.0 (c-real2prob 1.0)))
  (printf "random uniform 1-5: ~a\n" (uniform 1.0 500.0)))
