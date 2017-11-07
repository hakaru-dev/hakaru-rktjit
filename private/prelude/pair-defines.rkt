#lang racket

(require sham/ast
         (submod sham/ast utils))
(require "template-format.rkt"
         "type-defines.rkt"
         "utils.rkt")

(provide pair-defs
         pair-rator?
         get-pair-rator)

(define (pair-rator? sym)
  (member sym '(car cdr cons set-car! set-cdr!)))

(define (get-pair-rator sym tresult trands)
  (values
   (sham:rator:symbol
    (string->symbol
     (call-with-values
      (λ ()
        (match sym
          ['car (values pair-car-fun-format (get-type-string (car trands)))]
          ['cdr (values pair-cdr-fun-format (get-type-string (car trands)))]
          ['set-car! (values pair-set-car-fun-format (get-type-string (car trands)))]
          ['set-cdr! (values pair-set-cdr-fun-format (get-type-string (car trands)))]
          ['cons (values make-pair-fun-format (get-type-string tresult))]))

      format)))
   (void)))

(define (get-pair-types type)
  (match type
    [(sham:type:struct _ _ (list t1 t2)) (values t1 t2)]))

(define (pair-defs tast)
  (define-values
    (ptdefs ptdef pt ptref) (defs-def-t-tref tast))
  (define-values (atref btref) (get-pair-types pt))
  (define atp (sham:type:pointer atref))
  (define btp (sham:type:pointer btref))

  (define ptp (sham:type:pointer ptref))

  (define (get-fun-name frmt)
    (string->symbol (format frmt (sham:def-id ptdef))))

  (define (make-pair)
    (sham:def:function
     (prelude-function-info)
     (get-fun-name make-pair-fun-format) ;;make-pair
     '(a b) (list atref btref) ptp
     (sham:stmt:let
      '(pp ap bp)
      (list ptp atp btp)
      (list (sham$app malloc (sham:expr:type pt))
            (get-struct-field 'pp 0)
            (get-struct-field 'pp 1))
      (sham$block
       (sham:stmt:expr (sham$app store! a ap))
       (sham:stmt:expr (sham$app store! b bp))
       (sham:stmt:return (sham$var pp))))))

  (define (get-car)
    (sham:def:function
     (prelude-function-info)
     (get-fun-name pair-car-fun-format) ;;car
     '(p) (list ptp) atref
     (sham:stmt:return (sham$app load (get-struct-field 'p 0)))))

  (define (get-cdr)
    (sham:def:function
     (prelude-function-info)
     (get-fun-name pair-cdr-fun-format)
     '(p) (list ptp) btref
     (sham:stmt:return (sham$app load (get-struct-field 'p 1)))))

  (define (set-car)
    (sham:def:function
     (prelude-function-info)
     (get-fun-name pair-set-car-fun-format) ;;car
     '(p a) (list ptp atref) type-void-ref
     (sham$block
      (sham:stmt:expr
       (sham$app store! (sham$var a) (get-struct-field 'p 0)))
      (sham:stmt:return (sham:expr:void)))))


  (define (set-cdr)
    (sham:def:function
     (prelude-function-info)
     (get-fun-name pair-set-cdr-fun-format)
     '(p b) (list ptp btref) type-void-ref
     (sham$block
      (sham:stmt:expr
       (sham$app store! (sham$var b) (get-struct-field 'p 1)))
      (sham:stmt:return (sham:expr:void)))))


  (append
   (reverse ptdefs)
   (list (make-pair)
         (get-car) (get-cdr)
         (set-car) (set-cdr))))

(module+ test
  (require rackunit
           sham/jit
           ffi/unsafe)

  (define defs
    (apply append
           (map pair-defs
                `((pair nat nat)
                  (pair (pair nat nat) nat)))))
;  (pretty-print (map sham-def->sexp defs))
  (define mod
    (sham:module
     (build-info (basic-module-info)
                 '((ffi-libs . ((libgslcblas . ("libgslcblas" #:global? #t))
                                (libgsl . ("libgsl"))))))
     defs))
  (define cmod (compile-module mod))
  (jit-optimize-module cmod #:opt-level 3)
  (jit-dump-module cmod)
  (jit-verify-module cmod)
  (define cjmod (initialize-jit cmod))
  (define (get-t t) (jit-get-racket-type (env-lookup t cmod)))
  (define (get-f f) (jit-get-function f cmod))
  (define ((gf frmt) tsym)
    (get-f (get-fun-symbol frmt (get-type-string tsym))))

  (define t-nat (get-t 'nat))
  (define make-f (gf make-pair-fun-format))
  (define car-f (gf pair-car-fun-format))
  (define cdr-f (gf pair-cdr-fun-format))

  (define pnn `(pair nat nat))
  (define make-pair-nn (make-f pnn))
  (define car-pair-nn (car-f pnn))
  (define cdr-pair-nn (cdr-f pnn))
  (define tp (make-pair-nn 24 42))
  (check-eq? (car-pair-nn tp) 24)
  (check-eq? (cdr-pair-nn tp) 42)

  (define ppn `(pair (pair nat nat) nat))
  (define make-pair-ppn (make-f ppn))
  (define car-pair-ppn (car-f ppn))
  (define cdr-pair-ppn (cdr-f ppn))
  (define tpp (make-pair-ppn tp 84))
  (check-eq? (car-pair-nn (car-pair-ppn tpp)) 24)
  (check-eq? (cdr-pair-ppn tpp) 84))
