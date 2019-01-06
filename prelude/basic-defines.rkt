#lang racket

(require (for-syntax racket/syntax))

(require ffi/unsafe)

(require sham/private/ast-utils
         sham/private/jit-utils
         sham/private/ast-info
         sham/private/parameters
         "utils.rkt"
         "../ast.rkt"
         "type-defines.rkt"
         "template-format.rkt")
(provide (all-defined-out))


(define (basic-mod-info)
  (module-info-add-late-pass
   (module-info-add-ffi-libs
    (empty-module-info)
    `(libgslcblas . ("libgslcblas" #:global? #t))
    `(libgsl . ("libgsl")))
   'AlwaysInliner))

(define prelude-module  (create-empty-sham-module "prelude-module" (basic-mod-info)))
(current-sham-module prelude-module)

(define (prelude-function-info)
  (empty-function-info)
  ;; (function-info-add-attributes (empty-function-info) 'alwaysinline)
  )
(common-function-info (prelude-function-info))

(define-sham-function
  (prob2real (v : tprob)) : treal
  (ret  (ri^ exp.f64 tprob v)))

(define-sham-function
  (real2prob (v : treal)) : tprob
  (ret (ri^ log.f64 tprob v)))

(define-sham-function
  (nat2prob (v : tnat)) : tprob
 (ret (real2prob (ui->fp v (etype treal)))))

(define-sham-function
  (nat2real (v : tnat)) : treal
  (ret (ui->fp v (etype treal))))

(define-sham-function
  (nat2int (v : tnat)) : tint
  ;;NOTE: I think going from signed to unsigned is easy because of 2's complement
  ;; and we are using 64 bit so if the tnat is bigger than 2^32 then we have to worry
  ;; but for hakaru I think we don't need to worry as tnat's and int's don't go that big
  (ret v))

(define-sham-function
  ;; as in logfloatprelude of haskell this is implemented as id and is unsafe
  ;; the same is done here assuming the int is never going to be negative.
  (int2nat (v : tint)) : tnat
  (ret v))

(define-sham-function
  (int2real (v : tint)) : treal
  (ret (si->fp v (etype treal))))

(define-sham-function
  (recip-nat (v : tnat)) : treal
  (ret (fdiv (real-value 1.0)
             (ui->fp v (etype treal)))))
(define-sham-function
  (recip-real (v : treal)) : treal
  (ret (fdiv (real-value 1.0)  v)))

(define-sham-function
  (recip-prob (v : treal)) : treal
  (ret (fmul (real-value -1.0) v)))

(define-sham-function
  (root-prob-nat (v : tprob) (v2 : tnat)) : tprob
  (ret (fmul v (recip-nat v2))))

(define-sham-function
  (exp-real2prob (v : treal)) : tprob
  (ret v))

(define-sham-function
  (fdiv-nat (v1 : tnat) (v2 : tnat)) : treal
  (ret (fdiv
        (ui->fp v1 (etype treal))
        (ui->fp v2 (etype treal)))))

(define-sham-function
  (natpow (v : treal) (p : tnat)) : treal
  (ret (ri^ powi.f64 treal v (intcast p (etype i32)))))
(define-sham-function
  (natpow-prob (v : tprob) (p : tnat)) : tprob
  (ret (fmul v (nat2real p))))

(module+ test
  (require rackunit)
  (parameterize ([compile-options `(pretty dump mc-jit)])
    (compile-sham-module!
     (current-sham-module)
     #:opt-level 0))
  (check-= (sham-app natpow 4.0 3) 64 0.01))


(define simple-rators '(nat2prob nat2real prob2real real2prob int2real nat2int int2nat))
(define basic-rators
  (append simple-rators
          '(+ * < > / == - exp and not recip root reject natpow)))
(define basic-rator? (curryr member basic-rators))

(define (get-basic-rator rator tresult trands)
  (match rator
    ['< #:when (andmap (curry equal? 'nat) trands)
        icmp-ult]
    ['* (match trands
          ['(prob prob) fadd]
          ['(nat nat) mul-nuw]
          ['(int int) mul-nsw]
          ['(real real) fmul]
          ['(real real real) (λ (x y z) (fmul x (fmul y z)))])]
    ['+ (match trands
          ['(nat nat) add-nuw]
          ['(int int) add-nsw]
          ['(real real) fadd]
          ['(int int int) (λ (x y z) (add-nsw x (add-nsw y z)))]
          ['(real real real) (λ (x y z) (fadd x (fadd y z)))])]
    ['recip (match trands
              ['(nat) recip-nat]
              ['(real) recip-real]
              ['(prob) recip-prob])]
    ['== (match trands
           ['(nat nat) icmp-eq])]
    ['root (match trands
             ['(prob nat) root-prob-nat])]
    ['exp (match* (tresult trands)
            [('prob '(real)) exp-real2prob])]
    ['natpow
     (match (car trands)
       ['prob natpow-prob]
       ['real natpow])
     ;; (printf "get-basic-rator ~a\n" trands) (error "stop") natpow
     ]
    ['prob2real prob2real]
    ['real2prob real2prob]
    ['nat2prob nat2prob]
    ['nat2real nat2real]
    ['int2real int2real]
    ['int2nat int2nat]
    ['nat2int nat2int]))
#|
(define simple-rators '(nat2prob nat2real prob2real real2prob int2real nat2int int2nat))
(define simple-rator? (curryr member simple-rators))



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
                       (list
                        (for/fold ([body (sham:expr:app (sham:rator:symbol 'prob2real) (list (sham$var (get-vi 0))))])
                                  ([i (in-range 1 len)])
                          (sham:expr:app (sham:rator:symbol 'fadd)
                                         (list (sham:expr:app (sham:rator:symbol 'prob2real) (list (sham$var (get-vi i))))
                                               body))))))))

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
        (values (build-add-rator 'real trands)
                (build-add 'real trands))]
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

    ['-
     #:when (and (tnat? tresult)
                 (equal? (length trands) 2)
                 (tnat? (first trands))
                 (tnat? (second trands)))
     (values (sham:rator:symbol 'sub-nuw) (void))]

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
     (values (sham:rator:symbol (get-fun-symbol reject-fun-format tresult))
             (sham:def:function (prelude-function-info)
                                (get-fun-symbol reject-fun-format tresult) '() '() (get-sham-type-ref-ast tresult)
                                (ret
                                 (sham$uiv 0 (get-sham-type-ref-ast tresult)))))]

    [else (error "why is this basic-rator not figured out?"
                 sym tresult trands trslt trnds)]))




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

  ;; (define e 0.00000000001)
  ;; (check-= (c-nat2prob 8) (c-real2prob 8.0) e)
  ;; (check-= (c-real2prob 1.2345) (real->prob 1.2345) e)
  ;; (check-= (c-prob2real 1.2345) (prob->real 1.2345) e)

  ;; (check-= (recip-nat 2) 0.5 e)
  ;; (check-= (recip-real 5.2345) (/ 1.0 5.2345) e)
  ;; (check-= (recip-prob (c-real2prob 5.2345)) (real->prob (/ 1.0 5.2345)) e)
  ;; (check-= (recip-prob 14.124515) (real->prob (/ 1.0 (prob->real 14.124515))) e)
  ;; (check-= (prob->real (add3prob (c-nat2prob 4) (c-nat2prob 0) (c-nat2prob 1))) 5.0 e)

  ;; (check-eq? (add-2-nat 3 4) 7)
  ;; (check-= (add-2-real 1.234 543.1234) (+ 1.234 543.1234) e)
  ;; (check-= (add-3-real 5.324 543.2432 89.43241) (+ 5.324 543.2432 89.43241) e)
  ;; (check-= (add-2-prob 1.234 543.1234)
  ;;          (real->prob (+ (prob->real 1.234) (prob->real 543.1234)))
  ;;          e)
  )
;; (check-= (add-3-prob 5.324 543.2432 89.43241)
;;          (logspace-add 5.324 543.2432 89.43241)
;;          e)
;; (check-eq? (mul-2-nat 4 5) 20)
;; (check-= (mul-2-real 4.123 5.3123) (* 4.123 5.3123) e)
;; (check-= (mul-2-prob 4.123 5.3123) (+ 4.123 5.3123) e)
;; (check-= (mul-2-prob 4.123 5.3123)
;;          (real->prob (* (prob->real 4.123) (prob->real 5.3123)))
;;          e)
|#
