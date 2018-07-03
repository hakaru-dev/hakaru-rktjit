#lang racket

(require sham
         (submod sham/ast utils)
         math/distributions
         ffi/unsafe)
(require "template-format.rkt"
         "type-defines.rkt"
         "basic-defines.rkt"
         "array-defines.rkt"
         "utils.rkt")

(provide probability-defs
         probability-rator?
         build-categorical
         get-probability-rator)

(define (probability-rator? sym)
  (member sym '(uniform normal beta gamma
                        betafunc realbetafunc betafuncreal gammafunc
                        categorical
                        superpose-categorical superpose-categorical-real)))

(define (get-probability-rator sym tresult trands)
  (cond [(equal? sym 'superpose-categorical) (build-superpose-categorical tresult (length trands))]
        [(equal? sym 'superpose-categorical-real) (build-superpose-categorical-real tresult (length trands))]
        [(equal? sym 'categorical) (build-categorical (car trands))]
        [else (values (sham:rator:symbol sym) (void))]))

(define tvoid (sham:type:ref 'void))
(define tvoid* (sham:type:pointer (sham:type:ref 'i8)))
(define tnat (sham:type:ref 'nat))
(define treal (sham:type:ref 'real))
(define tprob (sham:type:ref 'prob))

(define (probability-defs)
  (define adefs (append
                 (array-defs `(array nat))
                 (array-defs `(array real))
                 (array-defs `(array prob))))
  (append
   adefs
   (list
    (sham:def:global (void) 'gsl-rng t8*)

    (sham$define
     #:info (prelude-function-info)
     (init-rng tvoid)
     (sham:stmt:block
      (list
       (sham:stmt:set!
        (sham:expr:var 'gsl-rng)
        (sham:expr:app (sham:rator:external 'libgsl 'gsl_rng_alloc tvoid*)
                       (list (sham:expr:external 'libgsl 'gsl_rng_rand tvoid*))))
       (sham:stmt:return (sham:expr:void)))))

    (sham$define
     #:info (prelude-function-info)
     (uniform  (v1 treal) (v2 treal) treal)
     (sham$block
      ;; (sham:stmt:expr
      ;;  (sham:expr:app
      ;;   (sham:rator:racket
      ;;    (gensym 'debug-uniform)
      ;;    (λ (a b) (printf "debug: uniform args: ~a, ~a\n" a b))
      ;;    (sham:type:function (list treal treal) (sham:type:ref 'void)))
      ;;   (list (sham$var v1) (sham$var v2))))
      (sham:stmt:return
       (sham:expr:app (sham:rator:external 'libgsl 'gsl_ran_flat treal)
                      (list (sham:expr:var 'gsl-rng)
                            (sham$var 'v1) (sham$var 'v2))))))

    (sham$define
     #:info (prelude-function-info)
     (normal  (mean treal) (sigma tprob) treal)
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
     #:info (prelude-function-info)
     (beta  (a tprob) (b tprob) tprob)
     (sham:stmt:return
      (sham:expr:app (sham:rator:symbol 'betafuncreal)
                     (list (sham:expr:app (sham:rator:symbol 'prob2real)
                                          (list (sham$var 'a)))
                           (sham:expr:app (sham:rator:symbol 'prob2real)
                                          (list (sham$var 'b)))))))


    (sham$define
     #:info (prelude-function-info)
     (realbetafunc  (a treal) (b treal) treal)
     (sham:stmt:return
      (sham:expr:app (sham:rator:symbol 'prob2real)
                     (list (sham:expr:app (sham:rator:symbol 'betafuncreal) (list (sham$var a) (sham$var  b)))))))

    (sham$define
     #:info (prelude-function-info)
     (betafuncreal  (a treal) (b treal) tprob)
     (sham:stmt:return (sham:expr:app (sham:rator:external 'libgsl 'gsl_sf_lnbeta treal)
                                      (list (sham$var a) (sham$var  b)))))

    (sham$define
     #:info (prelude-function-info)
     (betafunc  (a tprob) (b tprob) tprob)
     (sham:stmt:return
      (sham:expr:app (sham$rator betafuncreal)
                     (list (sham$app prob2real a) (sham$app prob2real b)))))

    (sham$define
     #:info (prelude-function-info)
     (gamma  (a tprob) (b tprob) tprob)
     (sham$block
      (sham:stmt:void)
      ;; (sham:stmt:expr (sham$app print-prob a))
      ;; (sham:stmt:expr (sham$app print-prob b))
      (sham:stmt:return
       (sham:expr:app (sham:rator:symbol 'real2prob)
                      (list
                       (sham:expr:app (sham:rator:external 'libgsl 'gsl_ran_gamma treal)
                                      (list (sham:expr:var 'gsl-rng)
                                            (sham:expr:app (sham:rator:symbol 'prob2real)
                                                           (list (sham$var 'a)))
                                            (sham:expr:app (sham:rator:symbol 'prob2real)
                                                           (list (sham$var 'b))))))))))

    (sham$define
     #:info (prelude-function-info)
     (gammafunc  (a treal) tprob)
     (sham:stmt:block
      (list
       (sham:stmt:expr
        (sham:expr:app
         (sham:rator:racket
          (gensym 'debug-uniform)
          (λ (a b) (printf "debug: gammafunc args: ~a\n" a b))
          (sham:type:function (list treal) (sham:type:ref 'void)))
         (list (sham$var a))))
       (sham:stmt:return
        (sham:expr:app (sham:rator:external 'libgsl 'gsl_sf_lngamma treal)
                       (list (sham$var 'a)))))))

    (sham:def:function
     (prelude-function-info)
     'print-prob (list 'a) (list tprob) (sham:type:ref 'void)
     (sham:stmt:block
      (list
       (sham:stmt:expr
        (sham:expr:app
         (sham:rator:racket
          'rkt-print
          (λ (a) (printf "printing-prob: ~a\n" a))
          (sham:type:function (list (sham:type:ref 'f64))
                              (sham:type:ref 'void)))
         (list (sham$var a))))
       (sham:stmt:return (sham:expr:void)))))

    (sham$define
     #:info (prelude-function-info)
     (categorical-real-disc

      (arr (sham:type:ref 'real*)) (size (sham:type:ref 'nat)) tnat)
     (sham:stmt:expr
      (sham:expr:let
       '(table) (list (sham:type:pointer (sham:type:ref 'i8)))
       (list
        (sham:expr:app (gsl-rator 'gsl_ran_discrete_preproc
                                  (sham:type:pointer (sham:type:ref 'i8)))
                       (list (sham$var size) (sham$var arr))))
       (sham:stmt:void)
       (sham:expr:let
        '(result) (list tnat)
        (list (sham:expr:app (gsl-rator 'gsl_ran_discrete tnat)
                             (list (sham:expr:var 'gsl-rng) (sham$var 'table))))
        (sham$block
         (sham:stmt:expr (sham:expr:app (gsl-rator 'gsl_ran_discrete_free tvoid) (list (sham$var 'table))))
         (sham:stmt:return (sham$var 'result)))
        (sham:expr:void)))))

    (sham$define
      #:info (prelude-function-info)
      (categorical-real
       (arr (sham:type:ref 'real*)) (size tnat) tnat)
      (sham$return
       (sham:expr:app
        (sham:rator:racket
         'cat
         (λ (arr len)
           (printf "categorical-array: ~a\n" (cblock->list arr _double len))
           (let ([val (sample (discrete-dist (build-list len identity) (cblock->list arr _double len)))])
             val))
         (sham:type:function (list (sham:type:ref 'real*)
                                   (sham:type:ref 'nat))
                             (sham:type:ref 'nat)))
        (list (sham$var arr)
              (sham$var size))))
      ;; (sham:stmt:expr
      ;;  (sham:expr:let
      ;;   '(i j) (list tnat tnat) (list (sham:expr:ui-value 1 tnat)
      ;;                                 (sham:expr:ui-value 0 tnat))
      ;;   (sham:stmt:block
      ;;    (list
      ;;     (sham:stmt:while
      ;;      (sham$app icmp-ult i size)
      ;;      (sham$block
      ;;       (sham:stmt:while
      ;;        (sham$app icmp-ult j i)
      ;;        (sham$block
      ;;         (sham:stmt:expr
      ;;          (sham$app store!
      ;;                    (sham$app fadd
      ;;                              (sham$app 'load arr (sham$var j))
      ;;                              (sham$app 'load arr (sham$var i)))
      ;;                    (sham:expr:gep (sham$var arr) (list (sham$var i)))))
      ;;         (sham:stmt:set! (sham$var j)
      ;;                         (sham$app add-nuw j (sham:expr:ui-value 1 tnat)))))
      ;;       (sham:stmt:set! (sham$var 'i)
      ;;                       (sham$app add-nuw i (sham:expr:ui-value 1 tnat)))))
      ;;     (sham:stmt:set! (sham$var i) (sham:expr:ui-value 0 tnat))))
      ;;   (sham:expr:let
      ;;    '(p) (list treal)
      ;;    (list (sham$app fmul
      ;;                    (sham$app 'load (sham:expr:gep (sham$var arr)
      ;;                                                   (list (sham$app sub-nuw size (sham:expr:ui-value 1 tnat)))))
      ;;                    (sham$app uniform (sham:expr:fl-value 0.0 treal) (sham:expr:fl-value 0.0 treal))))
      ;;    (sham:stmt:while
      ;;     (sham$app icmp-ult i size)
      ;;     (sham$block
      ;;      (sham:stmt:if
      ;;       (sham$app fcmp-ugt
      ;;                 (sham$app 'load
      ;;                           (sham:expr:gep
      ;;                            (sham$var arr) (list (sham$var i)))) p)
      ;;       (sham:stmt:return (sham$var i))
      ;;       (sham:stmt:void))
      ;;      (sham:stmt:set! (sham$var i)
      ;;                      (sham$app add-nuw i (sham:expr:ui-value 1 tnat)))))
      ;;    (sham:expr:let
      ;;     '() '() '()
      ;;     (sham:stmt:return (sham:expr:ui-value 0 tnat))
      ;;     (sham:expr:void)))))
      )


    (sham$define
     #:info (prelude-function-info)
     (categorical  (arp (sham:type:ref 'array<prob>*)) tnat)
     (sham:stmt:expr
      (sham:expr:let
       '(arr i mx)
       (list (sham:type:ref 'real*) type-nat-ref type-prob-ref)
       (list (sham$app arr-malloc (sham:expr:type type-real-ref)
                       (sham$app get-size$array<prob> arp))
             (sham:expr:ui-value 0 type-nat-ref)
             (sham$app get-index$array<prob> arp
                       (sham:expr:ui-value 0 type-nat-ref)))

       (sham$block
        (sham:stmt:while
         (sham$app icmp-ult i (sham$app get-size$array<prob> arp))
         (sham$block
          (sham:stmt:expr
           (sham:expr:let
            '(c) (list type-prob-ref)
            (list (sham$app get-index$array<prob> arp (sham$var 'i)))
            (sham:stmt:if (sham$app fcmp-uge (sham$var 'mx) (sham$var 'c))
                          (sham:stmt:set! (sham$var 'mx) (sham$var 'c))
                          (sham:stmt:void))
            (sham:expr:void)))
          (sham:stmt:set! (sham$var 'i)
                          (sham$app add-nuw i
                                    (sham:expr:ui-value 1 type-nat-ref)))))

        (sham:stmt:set! (sham$var 'i) (sham:expr:ui-value 0 type-nat-ref))

        (sham:stmt:while
         (sham$app icmp-ult i (sham$app get-size$array<prob> arp))
         (sham$block
          (sham:stmt:expr (sham$app store! (sham$app prob2real
                                                     (sham$app fsub
                                                               (sham$app get-index$array<prob> arp i)
                                                               (sham$var 'mx)))
                                    (sham:expr:gep (sham$var arr) (list (sham$var i)))))
          (sham:stmt:set! (sham$var 'i) (sham:expr:app (sham:rator:symbol 'add-nuw)
                                                       (list (sham$var 'i)
                                                             (sham:expr:ui-value  1 type-nat-ref)))))))
       (sham:expr:let
        '(result) (list tnat) (list (sham$app categorical-real-disc arr (sham$app get-size$array<prob> arp)))
        (sham$block
         (sham:stmt:expr (sham:expr:app (sham:rator:symbol 'free) (list (sham$var arr))))
         (sham:stmt:return (sham$var result)))
        (sham:expr:void))))))))

(define (gsl-rator sym type)
  (sham:rator:external 'libgsl sym type))

(define (build-categorical targ)
  (define fname (string->symbol (format "categorical$~a" (get-type-string targ))))
  (if (equal? targ `(array prob)) (values (sham:rator:symbol 'categorical) (void))
      (values
       (sham:rator:symbol fname)
       (match targ
         [`(array prob (size . ,s))
          (define get-index (sham:rator:symbol (string->symbol (format "get-index$array<~a.prob>" s))))
          (sham:def:function
           (prelude-function-info) fname
           (list 'arrs) (list (sham:type:ref (string->symbol (format "array<~a.prob>*" s)))) type-nat-ref
           (sham:stmt:expr
            (sham:expr:let
             '(arr i mx)
             (list (sham:type:ref 'real*) type-nat-ref type-prob-ref)
             (list (sham$app arr-alloca (sham:expr:type type-real-ref) (sham:expr:ui-value s type-nat-ref))
                   (sham:expr:ui-value 0 type-nat-ref)
                   (sham:expr:app get-index (list (sham$var arrs)
                                                  (sham:expr:ui-value 0 type-nat-ref))))
             (sham$block
              (sham:stmt:while
               (sham$app icmp-ult i (sham:expr:ui-value s type-nat-ref))
               (sham$block
                (sham:stmt:expr
                 (sham:expr:let
                  '(c) (list type-prob-ref)
                  (list (sham:expr:app get-index (list (sham$var arrs) (sham$var i))))
                  (sham:stmt:if (sham$app fcmp-uge (sham$var 'c) (sham$var 'mx))
                                (sham:stmt:set! (sham$var 'mx) (sham$var 'c))
                                (sham:stmt:void))
                  (sham:expr:void)))
                (sham:stmt:set! (sham$var 'i)
                                (sham$app add-nuw i
                                          (sham:expr:ui-value 1 type-nat-ref)))))

              (sham:stmt:set! (sham$var 'i) (sham:expr:ui-value 0 type-nat-ref))

              (sham:stmt:while
               (sham$app icmp-ult i (sham:expr:ui-value s type-nat-ref))
               (sham$block
                (sham:stmt:expr (sham$app store! (sham$app prob2real
                                                           (sham$app fsub
                                                                     (sham:expr:app get-index (list (sham$var arrs)
                                                                                                    (sham$var i)))
                                                                     (sham$var 'mx)))
                                          (sham:expr:gep (sham$var arr) (list (sham$var i)))))
                (sham:stmt:set! (sham$var 'i) (sham:expr:app (sham:rator:symbol 'add-nuw)
                                                             (list (sham$var 'i)
                                                                   (sham:expr:ui-value  1 type-nat-ref)))))))
             (sham:expr:let
              '() '() '()
              (sham:stmt:return (sham$app categorical-real-disc arr (sham:expr:ui-value s type-nat-ref)))
              (sham:expr:void)))))]))))

(define (build-superpose-categorical tresult len)
  ;;TODO we can probably optimize len two with binomial
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

(module+ test
  (require rackunit
           sham/jit
           ffi/unsafe
           "basic-defines.rkt")


  (define-values (_ sc2) (build-superpose-categorical 'bool 2))
  ;  (define-values (d cp10) (build-categorical `(array prob (size . 10))))
  (define defs (append (basic-defs)
                       (probability-defs)
                       (list sc2))); cp10)))

  ;(pretty-print (map sham-def->sexp defs))

  (define mod
    (sham:module (basic-mod-info) defs))
  (define cmod (compile-module mod))

  (jit-dump-module cmod)
  (jit-verify-module cmod)
  (optimize-module cmod #:opt-level 3)

  (initialize-jit! cmod)
  ;(pretty-print cmod)
  (printf "compiled and initialized")
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
  (define gamma (get-f 'gamma))

  (define beta (get-f 'beta))
  (define categorical-2 (get-f 'categorical-2))
  (define categorical-real (get-f 'categorical-real))
  (define categorical-real-disc (get-f 'categorical-real-disc))

  (define categorical-prob (get-f 'categorical))
  (define betaFunc (get-f 'betafunc))
  (define betaFuncreal (get-f 'betafuncreal))
  (define realbetaFunc (get-f 'realbetafunc))
  (define ars '(array real))
  (define arp '(array prob))
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

  (define make-array-prob (make-f arp))
  (define new-sized-array-prob (new-size-f arp))
  (define empty-array-prob (empty-f arp))
  (define get-data-array-prob (data-f arp))
  (define get-size-array-prob (size-f arp))
  (define get-index-array-prob (index-f arp))
  (define set-index-array-prob (index!-f arp))

  ;  (define t-real (get-t 'real))
  (printf"running tests")

  (define ta '(0.0 0.0 4.0))
  (define test-real-array (list->cblock ta t-real))
  (define test-arr (make-array-real (length ta) test-real-array))


  ;; (for ((i (in-range 10)))
  ;;    (define tr (list (c-real2prob 0.9)
  ;;                     (c-real2prob 0.1)))
  ;;    (define test-prob (make-array-prob (length tr) (list->cblock tr t-prob)))
  ;;    (printf "~a\n"
  ;;            (apply + (for/list ([i (in-range 500)])
  ;;                       (categorical-prob test-prob)))))
  ;; (0.21978021978021972 0.13333333333333333 0.0666666666666667)
  (define nb9900 '(-458.1908976216084 -461.9810000474454 -478.57190842283467 -446.5395681936905 -453.6747904210667 -463.4369314698437 -450.8619961395306 -453.16976113041795 -445.12265609048325 -374.5512758505581 -458.091812993318 -447.1539476558063 -454.3902748158387 -450.796948060257 -448.8193584047232 -450.2865595520607 -450.52765701408424 -460.69817293449074 -453.0247297820897 -457.1166709136238))
  (define nb99 '(268.0596789212606
                 272.15138815387155
                 272.15138815387155
                 272.15138815387155
                 274.6138467723381
                 272.15138815387155
                 193.6259482527394
                 256.24364009217334
                 272.15138815387155))
  (define nmax (apply max nb99))
  (define nbnn (map (λ (v) (- v nmax)) nb99))
  (define nb99real (map c-prob2real nbnn))
  (define tprob (make-array-prob (length nb9900) (list->cblock nb9900 t-prob)))
  (define tprob1 (make-array-prob (length nb99) (list->cblock nb99 t-prob)))
  (define gsllib (ffi-lib "libgsl"))

  (define preproc (get-ffi-obj "gsl_ran_discrete_preproc" gsllib (_fun _uint64 (_list i _double) -> _pointer)))
  (define rng_alloc (get-ffi-obj "gsl_rng_alloc" gsllib  (_fun _pointer -> _pointer)))
  (define rng_taus  (get-ffi-obj "gsl_rng_taus" gsllib _pointer))
  (define table (preproc (length nb99) nb99real))
  (define rng (rng_alloc rng_taus))

  (define discrete (get-ffi-obj "gsl_ran_discrete" gsllib (_fun _pointer _pointer -> _uint64)))
  (define discrete-pdf (get-ffi-obj "gsl_ran_discrete_pdf" gsllib (_fun _uint64 _pointer -> _double)))
  (printf "pdf: ~a\n" (for/list ([i (in-range (length nb99))]) (discrete-pdf i table)))

  ;; (begin
  ;;   (define pdf (make-hash))
  ;;   (define repeat 10000000)
  ;;   (for ([i (in-range repeat)])
  ;;     (define v (discrete rng table))
  ;;     (hash-set! pdf v (add1 (hash-ref pdf v 1))))
  ;;   (printf "pdfi: ~a\n" (for/list ([i (in-range (length nb99))])
  ;;                          (/ (hash-ref pdf i 0) (exact->inexact repeat)))))
  (printf "random: ~a\n" (for/list ([i (in-range 100)]) (discrete rng table)))
  (printf "categorical: ~a\n"  (for/list ([i (in-range 10)]) (categorical-prob tprob1)))
  (printf "categorical-real: ~a\n"  (for/list ([i (in-range 10)]) (categorical-real-disc (list->cblock nb99real t-real) (length nbnn))))

  ;; ;racket
  (require math/distributions)
  (define dd (discrete-dist (build-list (length nb99real) identity) nb99real))
  (sample (discrete-dist (build-list (length nb99real) identity) nb99real))
  (betaFunc (c-real2prob 4.0) (c-real2prob 4.0))
  ;; hkp logFromlogFloat $ betaFunc (prob_ 4.0) (prob_ 4.0)
  ;  (printf "cat-real : ~a\n" (categorical-real (get-data-array-real test-arr) (get-size-array-real test-arr)))

  ; (printf "cat2: ~a\n" (categorical-2 100.0 23.0))
  (printf "random normal mu=0, sd=1: ~a\n" (normal 0.0 (c-real2prob 1.0)))
  (printf "random uniform 1-5: ~a\n" (uniform 1.0 5.0)))
