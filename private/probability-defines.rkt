#lang racket

(require sham/ast)
(require "template-format.rkt"
         "type-defines.rkt"
         "array-defines.rkt"
         "basic-defines.rkt"
         "utils.rkt"
         "prelude.rkt")
(provide probability-defs
         build-superpose-categorical)

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
       (sham:stmt:return (sham:exp:void)))))

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

    (sham:def:function ;;TODO implement categorical using gsl_ran_discrete
     'categorical '() '()
     '(arr) (list (sham:type:ref 'array<prob>)) tnat
     (sham:stmt:return (nat-value 0))))))

(define (build-superpose-categorical len) ;;TODO we can probably optimize len two with binomial
  (define fun-name (string->symbol (format "categorical-~a" len)))
  (define func
    (sham:def:function
     fun-name '() '()
     (build-list len get-vi)
     (build-list len (const (sham:type:ref 'prob))) (sham:type:ref 'nat)
     (sham:stmt:expr
      (sham:exp:let (list 'arr)
                    (list (sham:type:ref 'array<prob>))
                    (list (sham:exp:app
                           (sham:rator:symbol
                            (string->symbol (format new-size-array-fun-format
                                                    'array<prob>)))
                           (list (nat-value len))))
                    (sham:stmt:block
                     (append
                      (for/list [(i (in-range len))]
                        (sham:stmt:expr
                         (sham:exp:app (sham:rator:symbol
                                        (string->symbol (format set-index-fun-format
                                                                'array<prob>)))
                                       (list (sham$var 'arr)
                                             (nat-value i)
                                             (sham$var (get-vi i))))))
                      (list
                       (sham:stmt:return
                        (sham:exp:app (sham:rator:symbol 'categorical)
                                      (list (sham$var 'arr)))))))
                    (sham:exp:void))))) ;;TODO free arr :P
  (values (cons func (array-defs '(array prob))) (sham:rator:symbol fun-name)))

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
  (printf "random uniform 1-5: ~a\n" (uniform 1.0 5.0)))
