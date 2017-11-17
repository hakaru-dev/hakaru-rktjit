#lang racket

(require (for-syntax racket/syntax))

(require ffi/unsafe)

(require sham
         (submod sham/ast utils)
         "utils.rkt"
         "../ast.rkt"
         "type-defines.rkt"
         "template-format.rkt")

(provide (all-defined-out))


(define (basic-defs)
  (define nat type-nat-ref)
  (define real type-real-ref)
  (define prob type-prob-ref)

  (list
   type-nat-def
   type-int-def
   type-real-def
   type-prob-def
   type-bool-def
   (sham$def:type nat* (sham:type:pointer type-nat-ref))
   (sham$def:type int* (sham:type:pointer type-int-ref))
   (sham$def:type real* (sham:type:pointer type-real-ref))
   (sham$def:type prob* (sham:type:pointer type-prob-ref))

   (sham$define #:info  (prelude-function-info)
    (nat2prob (v nat) prob)
    (sham$return
     (sham$app real2prob
               (sham$app ui->fp
                         v (sham$etype real)))))

   (sham$define #:info  (prelude-function-info)
    (nat2real (v nat) real)
    (sham$return
     (sham$app ui->fp v (sham$etype real))))


   (sham$define #:info  (prelude-function-info)
    (nat2int (v nat) int)
    ;;NOTE: I think going from signed to unsigned is easy because of 2's complement
    ;; and we are using 64 bit so if the nat is bigger than 2^32 then we have to worry
    ;; but for hakaru I think we don't need to worry as nat's and int's don't go that big
    (sham$return (sham$var v)))

   (sham$define #:info  (prelude-function-info)
    ;; as in logfloatprelude of haskell this is implemented as id and is unsafe
    ;; the same is done here assuming the int is never going to be negative.
    (int2nat (v int) nat)
    (sham$return (sham$var v)))

   (sham$define #:info  (prelude-function-info)
    (int2real (v int) real)
    (sham$return
     (sham$app si->fp v (sham$etype real))))

   (sham$define #:info  (prelude-function-info)
    (testput int)
    (sham$return (sham:expr:app (sham:rator:external 'libc 'putchar (sham:type:ref 'i32))
                                (list (sham:expr:ui-value 42 (sham:type:ref 'i32))))))

   (sham$define #:info  (prelude-function-info)
    (prob2real (v prob) real)
    (sham$return (sham$app (llvm.exp.f64 prob) v)))

   (sham$define #:info  (prelude-function-info)
    (real2prob (v real) prob)
    (sham$return (sham$app (llvm.log.f64 prob) v)))

   (sham$define #:info  (prelude-function-info)
    (recip-nat (v nat) real)
    (sham$return (sham$app fdiv (real-value 1.0)
                           (sham$app ui->fp v
                                     (sham$etype real)))))
   (sham$define #:info  (prelude-function-info)
    (recip-real (v real) real)
    (sham$return (sham$app fdiv (real-value 1.0)  v)))

   (sham$define #:info  (prelude-function-info)
    (recip-prob (v real) real)
    (sham$return (sham$app fmul (real-value -1.0)  v)))

   (sham$define #:info  (prelude-function-info)
    (root-prob-nat (v prob) (v2 nat) prob)
    (sham$return (sham$app fmul v (sham$app recip-nat v2))))

   (sham$define #:info  (prelude-function-info)
    (exp-real2prob (v real) prob)
    (sham$return (sham$var v)))

   (sham$define #:info  (prelude-function-info)
    (fdiv-nat (v1 nat) (v2 nat) real)
    (sham$return (sham$app fdiv
                           (sham$app ui->fp v1 (sham$etype real))
                           (sham$app ui->fp v2 (sham$etype real)))))

   (sham$define #:info  (prelude-function-info)
    (natpow (v real) (p nat) real)
    (sham$return (sham$app (llvm.powi.f64 real) v (sham$app intcast p (sham$etype i32)))))
   (sham$define #:info  (prelude-function-info)
    (natpow-prob (v prob) (p nat) prob)
    (sham$return (sham$app fmul v (sham$app nat2real p))))))


(define simple-rators '(nat2prob nat2real prob2real real2prob int2real nat2int int2nat))
(define simple-rator? (curryr member simple-rators))

(define basic-rators
  (append simple-rators
          '(+ * < > / == - exp and not recip root reject natpow)))

(define basic-rator? (curryr member basic-rators))

(define (add-prob-sym len)
  (string->symbol (format add-fun-format len 'prob)))

(define (build-add-prob len)
  (sham:def:function (prelude-function-info)
   (add-prob-sym len)
   (build-list len get-vi)
   (build-list len (const type-prob-ref)) type-prob-ref
   (sham:stmt:return
    (sham:expr:app
     (sham:rator:symbol 'real2prob)
     (list (sham:expr:app (sham:rator:symbol 'fadd)
            (build-list len (λ (vi)
                              (sham:expr:app (sham:rator:symbol 'prob2real)
                                            (list (sham$var (get-vi vi))))))))))))

(define (build-recip-nat->prob)
  (sham:def:function
   (prelude-function-info)
   'recip-nat->prob
   (build-list 2 get-vi)
   (build-list 2 (const type-nat-ref)) type-prob-ref
   (sham:stmt:return
    (sham$app nat2prob (sham$app fdiv
                                 (sham$app nat2real v0)
                                 (sham$app nat2real v1))))))

(define (build-eq-dif-type trands tresult)
  (error 'notdoneyet))

(define (build-add tresult trands)
  (define (get-add-fun)
    (match tresult
      ['nat 'add-nuw]
      ['int 'add-nsw]
      ['real 'fadd]
      ['prob 'fadd]))
  (define len (length trands))
  (define tref (sham:type:ref tresult))
  (sham:def:function
   (prelude-function-info)
   (get-add-sym len tresult)
   (build-list len get-vi)
   (build-list len (const tref)) tref
   (sham:stmt:return
    (for/fold ([body (sham$var (get-vi 0))])
              ([i (in-range 1 len)])
      (sham:expr:app (sham:rator:symbol (get-add-fun))
                     (list (sham$var (get-vi i))
                           body))))))

(define (build-mul tresult trands)
  (define (get-mul-fun)
    (match tresult
      ['nat 'mul-nuw]
      ['int 'mul-nsw]
      ['real 'fmul]))
  (define len (length trands))
  (define tref (sham:type:ref tresult))
  (sham:def:function
   (prelude-function-info)
   (get-mul-sym len tresult)
   (build-list len get-vi)
   (build-list len (const tref)) tref
   (sham:stmt:return
    (for/fold ([body (sham$var (get-vi 0))])
              ([i (in-range 1 len)])
      (sham:expr:app (sham:rator:symbol (get-mul-fun))
                     (list (sham$var (get-vi i))
                           body))))))

(define (get-add-sym len tresult)
  (get-fun-symbol add-fun-format len (get-type-string tresult)))
(define (get-mul-sym len tresult)
  (get-fun-symbol mul-fun-format len (get-type-string tresult)))

(define (build-add-rator tresult trands)
  (sham:rator:symbol (get-add-sym (length trands) tresult)))
(define (build-mul-rator tresult trands)
  (sham:rator:symbol (get-mul-sym (length trands) tresult)))

(define (get-basic-rator sym trslt trnds)
  (define tresult (if-need-pointer trslt))
  (define trands (map if-need-pointer trnds))
  (match sym
    [(? simple-rator?) (values (sham:rator:symbol sym) (void))]
    ['+ #:when (and (andmap tprob? trands) (tprob? tresult))
        (values (sham:rator:symbol (add-prob-sym (length trands)))
                (build-add-prob (length trands)))]
    ['* #:when (and (andmap tprob? trands) (tprob? tresult))
        (values (build-add-rator tresult trands)
                (build-add tresult trands))]

    ['* (values (build-mul-rator tresult trands) (build-mul tresult trands))]
    ['+ #:when (andmap (curry equal? tresult) trands)
        (values (build-add-rator tresult trands) (build-add tresult trands))]

    ['< #:when (andmap tnat? trands)
        (values (sham:rator:symbol 'icmp-ult) (void))]
    ['< #:when (andmap tint? trands)
        (values (sham:rator:symbol 'icmp-slt) (void))]

    ['/ #:when (and (andmap tnat? trands) (tnat? tresult))
        (values (sham:rator:symbol 'udiv) (void))]
    ['/ #:when (and (andmap tnat? trands) (treal? tresult))
        (values (sham:rator:symbol 'fdiv-nat) (void))]
    ['/ #:when (and (andmap tnat? trands) (tprob? tresult))
        (define def (build-recip-nat->prob))
        (values (sham:rator:symbol (sham:def-id def)) def)]

    ['== #:when (and (tbool? tresult) (andmap (curry equal? (car trands)) trands))
         (values (sham:rator:symbol 'icmp-eq) (void))]
    ['== #:when (tbool? tresult)
         (values (sham:rator:symbol (get-fun-symbol eq-dif-type trands))
                 (build-eq-dif-type trands tresult))]

    ['and #:when (andmap tbool? trands)
          (values (sham:rator:symbol 'and) (void))]
    ['not #:when (andmap tbool? trands)
          (values (sham:rator:symbol 'not) (void))]

    ['natpow
     #:when (and (tprob? tresult) (tprob? (first trands)) (tnat? (second trands)))
     (values (sham:rator:symbol 'natpow-prob) (void))]
    ['natpow
     #:when (and (treal? tresult) (treal? (first trands)) (tnat? (second trands)))
     (values (sham:rator:intrinsic 'llvm.powi.f64 (sham:type:ref 'real)) (void))]

    ['root
     #:when (and (tprob? tresult)
                 (equal? (length trands) 2)
                 ;(tprob? (first trands))
                 (tnat? (second trands)))
     (values (sham:rator:symbol 'root-prob-nat) (void))]

    ['exp
     #:when (and (tprob? tresult)
                 (equal? (length trands) 1)
                 (treal? (first trands)))
     (values (sham:rator:symbol 'exp-real2prob) (void))]

    ['recip
     (define (get-recip type)
       (match type
         ['nat 'recip-nat]
         ['real 'recip-real]
         ['prob 'recip-prob]))
     (values (sham:rator:symbol (get-recip tresult)) (void))]
    ['reject
     (define tr (if-need-pointer tresult))
     (values (sham:rator:symbol (get-fun-symbol reject-fun-format tr))
             (sham:def:function (prelude-function-info)
                                (get-fun-symbol reject-fun-format tr) '() '() (sham:type:ref tresult)
                                (sham$return (sham$uiv 0 (sham$tref tr)))))]

    [else (error "why is this basic-rator not figured out?"
                 sym tresult trands trslt trnds)]))


(define (basic-mod-info)
  (define mod-info (empty-mod-env-info))
  (mod-info-add-ffi-libs
   mod-info
   `(libgslcblas . ("libgslcblas" #:global? #t))
   `(libgsl . ("libgsl"))
   `(libc . ("/usr/lib/libc" "6")))
  (mod-info-add-passes
   mod-info
   'AlwaysInliner)
  mod-info)

(define (prelude-function-info)
  (define fn-attrs (fninfo-add-attrs (fninfo-empty) 'alwaysinline))
  fn-attrs)

(define (prog-fun-info arg-types ret-type fname)
  (define fn-attrs (fninfo-add-attrs (fninfo-empty) 'norecurse 'readonly 'argmemonly 'speculatable 'nounwind))
  (define ret-attrs (if (pointer-type? ret-type)
                        (fninfo-add-ret-attrs fn-attrs 'nonnull 'noalias)
                        fn-attrs))
  (for/fold ([info ret-attrs])
            ([arg-type arg-types]
             [i (in-range 1 (add1 (length arg-types)))])
    (if (pointer-type? arg-type)
        (fninfo-add-argi-attrs info i 'noalias 'nocapture 'nonnull)
        info)))

(module+ test
  (require rackunit)
  (require "../../utils.rkt")
  (define defs (append
                (basic-defs)
                (list (build-add-prob 3)
                      (build-add-prob 4))))
  (define mod (sham:module (basic-mod-info) defs))

  (define benv (compile-module mod))
  (optimize-module benv)
  (initialize-jit! benv)
  (jit-dump-module benv)
  (jit-verify-module benv)
  (define (get-t t) (jit-get-racket-type t benv))
  (define (get-f f) (jit-get-function f benv))

  (define t-real (get-t 'real))
  (define t-nat (get-t 'nat))
  (define t-prob (get-t 'prob))

  (define c-nat2prob (get-f 'nat2prob))
  (define c-nat2real (get-f 'nat2real))

  (define c-prob2real (get-f 'prob2real))
  (define c-real2prob (get-f 'real2prob))

  (define recip-nat (get-f 'recip-nat))
  (define recip-real (get-f 'recip-real))
  (define recip-prob (get-f 'recip-prob))

  (define add3prob (get-f 'add$3&prob))
  (define add4prob (get-f 'add$4&prob))
  ;; (define add-2-nat (get-f 'add-2-nat))
  ;; (define add-2-real (get-f 'add-2-real))
  ;; (define add-3-real (get-f 'add-3-real))
  ;; (define add-2-prob (get-f 'add-2-prob))
  ;; (define add-3-prob (get-f 'add-3-prob))

  ;; (define mul-2-nat (get-f 'mul-2-nat))
  ;; (define mul-2-real (get-f 'mul-2-real))
  ;; (define mul-2-prob (get-f 'mul-2-prob))

  (define e 0.00000000001)
  (check-= (c-nat2prob 8) (c-real2prob 8.0) e)
  (check-= (c-real2prob 1.2345) (real->prob 1.2345) e)
  (check-= (c-prob2real 1.2345) (prob->real 1.2345) e)

  (check-= (recip-nat 2) 0.5 e)
  (check-= (recip-real 5.2345) (/ 1.0 5.2345) e)
  (check-= (recip-prob (c-real2prob 5.2345)) (real->prob (/ 1.0 5.2345)) e)
  (check-= (recip-prob 14.124515) (real->prob (/ 1.0 (prob->real 14.124515))) e))

  ;; (check-eq? (add-2-nat 3 4) 7)
  ;; (check-= (add-2-real 1.234 543.1234) (+ 1.234 543.1234) e)
  ;; (check-= (add-3-real 5.324 543.2432 89.43241) (+ 5.324 543.2432 89.43241) e)
  ;; (check-= (add-2-prob 1.234 543.1234)
  ;;          (real->prob (+ (prob->real 1.234) (prob->real 543.1234)))
  ;;          e)
  ;; (check-= (add-3-prob 5.324 543.2432 89.43241)
  ;;          (logspace-add 5.324 543.2432 89.43241)
  ;;          e)
  ;; (check-eq? (mul-2-nat 4 5) 20)
  ;; (check-= (mul-2-real 4.123 5.3123) (* 4.123 5.3123) e)
  ;; (check-= (mul-2-prob 4.123 5.3123) (+ 4.123 5.3123) e)
  ;; (check-= (mul-2-prob 4.123 5.3123)
  ;;          (real->prob (* (prob->real 4.123) (prob->real 5.3123)))
  ;;          e)
