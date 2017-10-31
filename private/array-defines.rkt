#lang racket

(require sham/ast)
(require "template-format.rkt"
         "type-defines.rkt"
         "prelude.rkt")

(provide array-defs)

(define (get-array-data-type type)
  (match type
    [(sham:type:struct _ (list _ t)) t]))

;;tast be expanded
(define (array-defs tast)
  (define-values
    (atdefs atdef at atref) (defs-def-t-tref tast))
  (define-values
    (aptdefs aptdef apt aptref) (defs-def-t-tref `(pointer ,tast)))
  (define adtref (get-array-data-type at))
  (define adpt (sham:type:pointer adtref))
  (define adnptref (type-remove-pointer adtref))
  (define nat type-nat-ref)
  (define nat* (sham:type:pointer nat))
  (define arr-id (sham:def-id atdef))
  (define (get-fun-name frmt)
    (string->symbol (format frmt arr-id)))

  (define (make-array)
    (sham:def:function ;make-array
     (get-fun-name make-array-fun-format)
     '() '(AlwaysInline)
     array-args (list nat adtref) aptref
     (sham:stmt:let
      '(ap* ap-size* ap-data*)
      (list aptref nat* adpt)
      (list (sham$app malloc (sham:exp:type atref))
            (get-size-ptr 'ap*)
            (get-data-ptr 'ap*))
      (sham$block
       (sham:stmt:exp (sham$app-var store! size ap-size*))
       (sham:stmt:exp (sham$app-var store! data ap-data*))
       (sham:stmt:return (sham$var 'ap*))))))
  ;; (pretty-print (sham-def->sexp (make-array)))

  (define (get-array-data)
    (sham:def:function ;get-array-data
     (get-fun-name get-array-data-fun-format)
     '() '(AlwaysInline)
     '(ap*) (list aptref) adtref
     (sham:stmt:let
      '(adt*)
      (list adpt)
      (list (get-data-ptr 'ap*))
      (sham:stmt:return (sham$app load (sham$var 'adt*))))))
  (define (new-size-array)
    (sham:def:function ;new-size-array
     (get-fun-name new-size-array-fun-format)
     '() '(AlwaysInline)
     '(size) (list nat) aptref
     (sham:stmt:return (sham:exp:app
                        (sham:rator:symbol (get-fun-name make-array-fun-format))
                        (list (sham$var 'size)
                              (sham$app arr-malloc
                                        (sham:exp:type adnptref) (sham$var 'size)))))))
  (define (get-array-size)
    (sham:def:function ;get-array-size
     (get-fun-name get-array-size-fun-format)
     '() '(AlwaysInline)
     '(array-ptr) (list aptref) nat
     (sham:stmt:return (sham$app load (get-size-ptr 'array-ptr)))))

  (define (get-index)
    (sham:def:function ;get-index
     (get-fun-name get-index-fun-format)
     '() '(AlwaysInline)
     '(array-ptr index)
     (list aptref nat ) adnptref
     (sham:stmt:return
      (sham$app load
                (sham:exp:gep (sham$app load (get-data-ptr 'array-ptr))
                              (list (sham$var 'index)))))))
  (define (set-index)
    (sham:def:function ;set-index
     (get-fun-name set-index-fun-format)
     '() '(AlwaysInline)
     '(array-ptr index v)
     (list aptref nat adnptref) (sham:type:ref 'void)
     (sham$block
      (sham:stmt:exp-stmt
       (sham$app store! (sham$var 'v)
                 (sham:exp:gep (sham$app load (get-data-ptr 'array-ptr))
                               (list (sham$var 'index))))
       (sham:stmt:void))
      (sham:stmt:return-void))))
  (define (empty-array)
    (sham:def:function ;empty-array
     (get-fun-name empty-array-fun-format)
     '() '(AlwaysInline)
     '()
     (list ) aptref
     (sham:stmt:return
      (sham:exp:app (sham:rator:symbol (get-fun-name new-size-array-fun-format))
                    (list (nat-value 0))))))
  (append
   (reverse atdefs)
   (reverse aptdefs)
   (list
    (make-array)
    (new-size-array)
    (empty-array)
    (get-array-size)
    (get-array-data)
    (get-index)
    (set-index))))

(module+ test
  (require rackunit
           sham/jit
           ffi/unsafe
           "utils.rkt")

  (define defs
    (apply append
           (map array-defs
                `((array nat)
                  (array real)
                  (array prob)
                  (array (array nat))))))
  (pretty-print (map sham-def->sexp defs))
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
  (define e 0.00000000001)

  (define ta '(1.0 2.0 3.0 3.14 42.23))
  (define ti '(1 2 3 4 42))
  (define test-nat-array (list->cblock ti t-nat))
  (define test-real-array (list->cblock ta t-real))
  (define test-prob-array (list->cblock (map real->prob ta) t-prob))

  (define ans '(array nat))
  (define ars '(array real))
  (define aps '(array prob))
  (define aans '(array (array nat)))
  (define ((gf frmt) tsym)
    (get-f (get-fun-symbol frmt (get-type-string tsym))))

  (define make-f (gf make-array-fun-format))
  (define new-size-f (gf new-size-array-fun-format))
  (define empty-f (gf empty-array-fun-format))
  (define size-f (gf get-array-size-fun-format))
  (define data-f (gf get-array-data-fun-format))
  (define index-f (gf get-index-fun-format))
  (define index!-f (gf set-index-fun-format))

  (define make-array-nat (make-f ans))
  (define new-sized-array-nat (new-size-f ans))
  (define empty-array-nat (empty-f ans))
  (define get-data-array-nat (data-f ans))
  (define get-size-array-nat (size-f ans))
  (define get-index-array-nat (index-f ans))
  (define set-index-array-nat (index!-f ans))

  (define tiarr (make-array-nat (length ti) test-nat-array))
  (define eti (new-sized-array-nat 5))
  (check-eq? (get-size-array-nat tiarr) 5)
  (check-eq? (get-index-array-nat tiarr 4) 42)
  (set-index-array-nat tiarr 3 23)
  (check-eq? (get-index-array-nat tiarr 3) 23)

  (check-eq? (get-size-array-nat eti) 5)
  (set-index-array-nat eti 3 42)
  (check-eq? (get-index-array-nat eti 3) 42)

  (define make-array-real (make-f ars))
  (define new-sized-array-real (new-size-f ars))
  (define empty-array-real (empty-f ars))
  (define get-data-array-real (data-f ars))
  (define get-size-array-real (size-f ars))
  (define get-index-array-real (index-f ars))
  (define set-index-array-real (index!-f ars))

  (define trarr (make-array-real (length ti) test-real-array))
  (define etr (new-sized-array-real 5))
  (check-eq? (get-size-array-real trarr) 5)
  (check-= (get-index-array-real trarr 4) 42.23 e)
  (set-index-array-real trarr 3 23.42)
  (check-= (get-index-array-real trarr 3) 23.42 e)

  (check-eq? (get-size-array-real etr) 5)
  (set-index-array-real etr 3 42.23)
  (check-= (get-index-array-real etr 3) 42.23 e)

  (define make-array-prob (make-f aps))
  (define new-sized-array-prob (new-size-f aps))
  (define empty-array-prob (empty-f aps))
  (define get-data-array-prob (data-f aps))
  (define get-size-array-prob (size-f aps))
  (define get-index-array-prob (index-f aps))
  (define set-index-array-prob (index!-f aps))

  (define tparr (make-array-prob (length ti) test-prob-array))
  (define etp (new-sized-array-prob 5))
  (check-eq? (get-size-array-prob tparr) 5)
  (check-= (get-index-array-prob tparr 4) (real->prob 42.23) e)
  (set-index-array-prob tparr 3 23.42)
  (check-= (get-index-array-prob tparr 3) 23.42 e)

  (check-eq? (get-size-array-prob etp) 5)
  (set-index-array-prob etp 3 42.23)
  (check-= (get-index-array-prob etp 3) 42.23 e)

  (define make-array-array-nat (make-f aans))
  (define new-sized-array-array-nat (new-size-f aans))
  (define empty-array-array-nat (empty-f aans))
  (define get-data-array-array-nat (data-f aans))
  (define get-size-array-array-nat (size-f aans))
  (define get-index-array-array-nat (index-f aans))
  (define set-index-array-array-nat (index!-f aans))
  (define etai (new-sized-array-array-nat 5))
  (set-index-array-array-nat etai 2 eti)
  (define etib (get-index-array-array-nat etai 2))
  (check ptr-equal? etib eti)
  (check-eq? (get-index-array-nat etib 3) 42))
