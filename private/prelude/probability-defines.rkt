#lang racket

(require sham/ast
         (submod sham/ast utils))
(require "template-format.rkt"
         "type-defines.rkt"
         "array-defines.rkt"
         "utils.rkt")

(provide probability-defs
         get-probability-rator)

(define (get-probability-rator sym tresult trands)
  (if (equal? sym 'superpose-categorical)
      (build-superpose-categorical (length trands))
      (values
       (sham:rator:symbol
        (string->symbol
         (call-with-values
          (λ ()
            (match sym
              ['car (values pair-car-fun-format (get-type-string (car trands)))]
              ['cdr (values pair-cdr-fun-format (get-type-string (car trands)))]
              ['cons (values make-pair-fun-format (get-type-string tresult))]))
          format)))
       (void))))

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
     (sham:stmt:return
      (sham:expr:app (sham:rator:symbol 'real2prob)
                    (list
                     (sham:expr:app (sham:rator:external 'libgsl 'gsl_ran_beta treal)
                                   (list (sham:expr:var 'gsl-rng)
                                         (sham:expr:app (sham:rator:symbol 'prob2real)
                                                       (list (sham$var 'a)))
                                         (sham:expr:app (sham:rator:symbol 'prob2real)
                                                       (list (sham$var 'b)))))))))
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

    (sham$define ;;TODO implement categorical using gsl_ran_discrete
     (categorical (prelude-function-info) (arr (sham:type:ref 'array<prob>)) tnat)
     (sham:stmt:return (nat-value 0))))))

(define (build-superpose-categorical len) ;;TODO we can probably optimize len two with binomial
  (define fun-name (string->symbol (format "categorical-~a" len)))
  (define func
    (sham:def:function
     fun-name '() '()
     (build-list len get-vi)
     (build-list len (const (sham:type:ref 'prob))) (sham:type:ref 'nat)
     (sham:stmt:expr
      (sham:expr:let (list 'arr)
                    (list (sham:type:ref 'array<prob>))
                    (list (sham:expr:app
                           (sham:rator:symbol
                            (string->symbol (format new-size-array-fun-format
                                                    'array<prob>)))
                           (list (nat-value len))))
                    (sham:stmt:block
                     (append
                      (for/list [(i (in-range len))]
                        (sham:stmt:expr
                         (sham:expr:app (sham:rator:symbol)
                                        (string->symbol (format set-index-fun-format
                                                                'array<prob>))
                                       (list (sham$var 'arr)
                                             (nat-value i)
                                             (sham$var (get-vi i))))))
                      (list
                       (sham:stmt:return
                        (sham:expr:app (sham:rator:symbol 'categorical)
                                      (list (sham$var 'arr)))))))
                    (sham:expr:void))))) ;;TODO free arr :P
  (values (sham:rator:symbol fun-name func)))
;  (values (cons func (array-defs '(array prob))) (sham:rator:symbol fun-name)))

(module+ test
  (require rackunit
           sham/jit
           ffi/unsafe
           "basic-defines.rkt")


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