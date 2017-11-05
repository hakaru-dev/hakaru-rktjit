#lang racket

(require (for-syntax racket/syntax))

(require ffi/unsafe)

(require sham/jit
         sham/ast
         "../utils.rkt"
         "../ast.rkt"
         "type-defines.rkt"
         "template-format.rkt")

(provide (all-defined-out))

(define (basic-defs)
  (define nat type-nat-ref)
  (define nat* (sham:type:ref 'nat*))
  (define real type-real-ref)
  (define prob type-prob-ref)

  (list
   type-nat-def
   type-int-def
   type-real-def
   type-prob-def
   (sham:def:type 'nat* (sham:type:pointer type-nat-ref))
   (sham:def:type 'int* (sham:type:pointer type-int-ref))
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
    (nat2int (v : nat) : int)
    ;;NOTE: I think going from signed to unsigned is easy because of 2's complement
    ;; and we are using 64 bit so if the nat is bigger than 2^32 then we have to worry
    ;; but for hakaru I think we don't need to worry as nat's and int's don't go that big
    (return (sham$var 'v)))

   (sham$define
    (int2real (v : int) : real)
    (return
     (sham$app si->fp (sham$var 'v) (sham:exp:type (sham:type:ref 'real)))))

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

(define (simple-rator? sym)
  (member sym
          '(nat2prob nat2real prob2real real2prob
                     int2real nat2int
                     and not
                     uniform normal beta gamma categorical)))
                     ;recip-nat recip-real recip-prob)))
(define math-rator?
  (curryr member '(+ * < > / == - natpow recip root)))

(define (add-prob-sym len)
  (string->symbol (format add-fun-format len 'prob)))
(define (build-add-prob len)
  (sham:def:function
   (add-prob-sym len) '() '()
   (build-list len get-vi)
   (build-list len (const type-prob-ref)) type-prob-ref
   (sham:stmt:return
    (sham:exp:app
     (sham:rator:symbol 'real2prob)
     (list (sham:exp:app (sham:rator:symbol 'fadd)
            (build-list len (λ (vi)
                              (sham:exp:app (sham:rator:symbol 'prob2real)
                                            (list (sham$var (get-vi vi))))))))))))



(define (figure-out-math sym rands tresult trands)
  (match sym
    ['* #:when (and (andmap tprob? trands))
        (values '() (sham:rator:symbol 'fadd) rands)]
    ['* #:when (and (andmap treal? trands))
        (values '() (sham:rator:symbol 'fmul) rands)]
    ['* #:when (and (andmap tnat? trands))
        (values '() (sham:rator:symbol 'mul-nuw) rands)]
    ['* #:when (and (andmap tint? trands))
        (values '() (sham:rator:symbol 'mul-nsw) rands)]

    ['+ #:when (and (andmap treal? trands))
        (values '() (sham:rator:symbol 'fadd) rands)]
    ['+ #:when (and (andmap tnat? trands))
        (values '() (sham:rator:symbol 'add-nuw) rands)]
    ['+ #:when (and (andmap tint? trands))
        (values '() (sham:rator:symbol 'add-nsw) rands)]
    ['+ #:when (and (andmap tprob? trands) (tprob? tresult))
        (values (list (build-add-prob (length trands)))
                (sham:rator:symbol (add-prob-sym (length trands)))
                rands)]

    ['< #:when (andmap tnat? trands)
        (values '() (sham:rator:symbol 'icmp-ult) rands)]
    ['/ #:when (and (andmap tnat? trands) (tnat? tresult))
        (values '() (sham:rator:symbol 'udiv) rands)]
    ['/ #:when (and (andmap tnat? trands) (equal? (length trands) 2)
                    (tprob? tresult))
        (values '()
                (sham:rator:symbol 'fadd)
                (list (expr-app 'prob (expr-intrf 'real2prob)
                                (list (expr-app 'real (expr-intrf 'nat2real)
                                                (list (first rands)))))
                      (expr-app 'prob (expr-intrf 'recip)
                                (list
                                 (expr-app 'prob (expr-intrf 'real2prob)
                                           (list (expr-app 'real
                                                           (expr-intrf 'nat2real)
                                                           (list (second rands)))))))))]

    ['== #:when (andmap tnat? trands)
         (values '() (sham:rator:symbol 'icmp-eq) rands)]
    ['natpow
     #:when (and (treal? tresult) (treal? (first trands)) (tnat? (second trands)))
     (values '() (sham:rator:intrinsic 'llvm.powi.f64 (sham:type:ref 'real)) rands)]
    ['root
     #:when (and (tprob? tresult)
                 (equal? (length trands) 2)
                 (tprob? (first trands))
                 (tnat? (second trands)))
     (values (list (sham$define (root-prob-nat (v : prob) (v2 : nat) : prob)
                                (return (sham$app fmul  (sham$var 'v)
                                                  (sham$app-var recip-nat v2)))))
             (sham:rator:symbol 'root-prob-nat)
             rands)]
    ['recip
     (define (get-recip type)
       (match type
         ['nat 'recip-nat]
         ['real 'recip-real]
         ['prob 'recip-prob]))
     (values '() (sham:rator:symbol (get-recip tresult)) rands)]
    [else (printf "why is this math not figured out?: ~a, tresult: ~a, trands: ~a\n"
                  sym tresult trands)
          (values '() (sham:rator:symbol (symbol-append sym '?)) rands)]))


(define (basic-mod-info)
  '((passes . ())
    (ffi-libs . ((libgslcblas . ("libgslcblas" #:global? #t))
                 (libgsl . ("libgsl"))))))
(module+ test
  (require rackunit)
  (require "../../utils.rkt")
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
