#lang racket

(require (for-syntax racket/syntax))

(require ffi/unsafe)

(require sham/jit
         sham/ast)

(require "sham-utils.rkt")
(provide basic-defines)

;;TODO: make function creation on demand for compiling program
(define i32 (sham:type:ref 'i32))
(define f64 (sham:type:ref 'f64))
(define nat i32)
(define int i32)
(define real f64)
(define prob f64)
(define (create-pointer-type t-sym)
  (sham:type:pointer (sham:type:ref t-sym)))
(define (create-array-type t)
  (sham:type:struct
   '(size data)
   (list nat 
         (cond [(symbol? t) (sham:type:ref t)]
               [else t]))))

(define-syntax (create-ptr-def stx)
  (syntax-case stx ()
    [(_ ptr)
     (with-syntax ([ptr* (format-id stx "~a*"  #'ptr)])
       #'(sham:def:type 'ptr* (create-pointer-type 'ptr)))]))
(define-syntax (create-ptr-defs stx)
  (syntax-case stx ()
    [(_ ptr ...)
     #'(list (create-ptr-def ptr) ...)]))

(define-syntax (create-array-def stx)
  (syntax-case stx ()
    [(_ type)
     (with-syntax ([type* (format-id stx "~a*"  #'type)]
                   [atype (format-id stx "array<~a>" #'type)])
       #'(sham:def:type 'atype (create-array-type 'type*)))]))
(define-syntax (create-array-defs stx)
  (syntax-case stx ()
    [(_ type ...)
     #'(list (create-array-def type) ...)]))

(define types
  (append
   (list
    (sham:def:type 'nat  (sham:type:ref 'i32))
    (sham:def:type 'int  (sham:type:ref 'i32))
    (sham:def:type 'real (sham:type:ref 'f64))
    (sham:def:type 'prob (sham:type:ref 'f64)))
   (create-ptr-defs
    nat nat* nat**
    real real* real**
    prob prob* prob**)
   (create-array-defs real prob nat)
   (create-ptr-defs array<nat> array<real> array<prob>)
   (create-array-defs array<nat> array<real> array<prob>)))
(define simple-funs
  (list
   (sham$define
    (nat2prob (v : nat) : prob)
    (return
     (sham$app ui->fp (sham$var 'v) (sham:exp:type (sham:type:ref 'real)))))

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
    (return (sham$app fmul (real-value -1.0) (sham$var 'v))))

   (sham$define
    (add-2-nat (v1 : nat) (v2 : nat) : nat)
    (return (sham$app-var add-nuw v1 v2)))
   (sham$define
    (add-2-real (v1 : real) (v2 : real) : real)
    (return (sham$app-var fadd v1 v2)))
   (sham$define
    (add-3-real (v1 : real) (v2 : real) (v3 : real) : real)
    (return (sham$app fadd
                      (sham$app-var fadd v1 v2)
                      (sham$var 'v3))))

   (sham$define
    (add-2-prob (v1 : prob) (v2 : prob) : prob)
    (return (sham$app real2prob
                      (sham$app add-2-real
                                (sham$app-var prob2real v1)
                                (sham$app-var prob2real v2)))))

   (sham$define
    (add-3-prob (v1 : prob) (v2 : prob) (v3 : prob) : prob)
    (return (sham$app real2prob
                      (sham$app add-3-real
                                (sham$app-var prob2real v1)
                                (sham$app-var prob2real v2)
                                (sham$app-var prob2real v3)))))
   (sham$define
    (mul-2-nat (v1 : nat) (v2 : nat) : nat)
    (return (sham$app-var mul-nuw v1 v2)))
   (sham$define
    (mul-2-real (v1 : real) (v2 : real) : real)
    (return (sham$app-var fmul v1 v2)))
   (sham$define
    (mul-2-prob (v1 : prob) (v2 : prob) : prob)
    (return (sham$app-var fadd v1 v2)))

   (sham$define
    (mul-4-prob (v1 : prob) (v2 : prob) (v3 : prob) (v4 : prob) : prob)
    (return (sham$app fadd
                      (sham$var 'v4)
                      (sham$app fadd (sham$var 'v3) (sham$app-var fadd v1 v2)))))))

(define (get-size-ptr vsym)
  (sham:exp:gep (sham$var vsym) (list (nat-value 0) (nat-value 0))))
(define (get-data-ptr vsym)
  (sham:exp:gep (sham$var vsym) (list (nat-value 0) (nat-value 1))))
;TODO make a sham util to have templated types
(define (make-array type)
  (define fn-name (string->symbol (format "make-array<~a>" type)))
  (define p-type (create-pointer-type type))
  (define a-type (create-array-type p-type))
  (define a-type* (sham:type:pointer a-type))
  (define p-type* (sham:type:pointer p-type))
  (sham:def:function
   fn-name '() '(AlwaysInline)
   '(size data)
   (list i32 p-type) a-type*
   (sham:stmt:let
    '(ap ap-size* ap-data*)
    (list a-type* (create-pointer-type 'i32)
          p-type*)
    (list (sham$app malloc (sham:exp:type a-type))
          (get-size-ptr 'ap)
          (get-data-ptr 'ap))
    (sham$block
     (sham:stmt:exp (sham$app-var store! size ap-size*))
     (sham:stmt:exp (sham$app-var store! data ap-data*))
     (sham:stmt:return (sham$var 'ap))))))

(define (get-array-type type)
  (define fn-name (string->symbol (format "get-ptr-array<~a>" type)))
  (define p-type (create-pointer-type type))
  (define a-type (create-array-type p-type))
  (define a-type* (sham:type:pointer a-type))
  (define p-type* (sham:type:pointer p-type))
  (sham:def:function
   fn-name '() '(AlwaysInline)
   '(ap)
   (list a-type*) p-type
   (sham:stmt:let
    '(atp)
    (list p-type*)
    (list (get-data-ptr 'ap))
    (sham:stmt:return (sham$app load (sham$var 'atp))))))

(define (empty-array-type type)
  (define fn-name (string->symbol (format "empty-array<~a>" type)))
  (define p-type (create-pointer-type type))
  (define a-type (create-array-type p-type))
  (define a-type* (sham:type:pointer a-type))
  (define p-type* (sham:type:pointer p-type))
  (sham:def:function
   fn-name '() '(AlwaysInline)
   '(size)
   (list i32) a-type*
   (sham:stmt:let
    '(ap data datap sizep)
    (list a-type* p-type p-type* (sham:type:pointer i32))
    (list (sham$app malloc (sham:exp:type a-type))
          (sham$app arr-malloc (sham:exp:type (sham:type:ref type)) (sham$var 'size))
          (get-data-ptr 'ap)
          (get-size-ptr 'ap))
    (sham$block
     (sham:stmt:exp (sham$app-var store! size sizep))
     (sham:stmt:exp (sham$app-var store! data datap))
     (sham:stmt:return (sham$var 'ap))))))

(define (size-array-type type)
  (define fn-name (string->symbol (format "size-array<~a>" type)))
  (define p-type (create-pointer-type type))
  (define a-type (create-array-type p-type))
  (define a-type* (sham:type:pointer a-type))
  (define p-type* (sham:type:pointer p-type))
  (sham:def:function
   fn-name '() '(AlwaysInline)
   '(array-ptr)
   (list a-type*) i32
   (sham:stmt:return (sham$app load (get-size-ptr 'array-ptr)))))

(define (index-array-type type)
  (define fn-name (string->symbol (format "index-array<~a>" type)))
  (define p-type (create-pointer-type type))
  (define a-type (create-array-type p-type))
  (define a-type* (sham:type:pointer a-type))
  (define p-type* (sham:type:pointer p-type))
  (sham:def:function
   fn-name '() '(AlwaysInline)
   '(array-ptr index)
   (list a-type* i32) (sham:type:ref type)
   (sham:stmt:return
    (sham$app load
              (sham:exp:gep (sham$app load (get-data-ptr 'array-ptr))
                            (list (sham$var 'index)))))))

(define (set-array-type-at-index type)
  (define fn-name (string->symbol (format "set-index-in-array<~a>" type)))
  (define p-type (create-pointer-type type))
  (define a-type (create-array-type p-type))
  (define a-type* (sham:type:pointer a-type))
  (define p-type* (sham:type:pointer p-type))
  (sham:def:function
   fn-name '() '(AlwaysInline)
   '(array-ptr index v)
   (list a-type* i32 (sham:type:ref type)) (sham:type:ref 'void) 
   (sham$block
    (sham:stmt:exp
     (sham$app store! (sham$var 'v)
               (sham:exp:gep (sham$app load (get-data-ptr 'array-ptr))
                             (list (sham$var 'index)))))
    (sham:stmt:return-void))))

(define (empty-array-type-zero type)
  (define fn-name (string->symbol (format "empty-zero-array<~a>" type)))
  (define p-type (create-pointer-type type))
  (define a-type (create-array-type p-type))
  (define a-type* (sham:type:pointer a-type))
  (define p-type* (sham:type:pointer p-type))
  (sham:def:function
   fn-name '() '(AlwaysInline)
   '()
   (list ) a-type*
   (sham:stmt:return
    (sham:exp:app (sham:rator:symbol (string->symbol (format "empty-array<~a>" type)))
              (list (nat-value 0))))))

(define array-functions
  (apply
   append
   (for/list [(t '(nat real prob array<nat>*))]
     (list
      (make-array t)
      (get-array-type t)
      (empty-array-type t)
      (size-array-type t)
      (index-array-type t)
      (set-array-type-at-index t)
      (empty-array-type-zero t)))))

(define (basic-defines)
  (append types simple-funs array-functions))

(module+ test
  (require rackunit)
  (require "utils.rkt")
  (define benv (initialize-jit (compile-module (sham:module '() (basic-defines)))))
  
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

  (define add-2-nat (get-f 'add-2-nat))
  (define add-2-real (get-f 'add-2-real))
  (define add-3-real (get-f 'add-3-real))
  (define add-2-prob (get-f 'add-2-prob))
  (define add-3-prob (get-f 'add-3-prob))
  
  (define mul-2-nat (get-f 'mul-2-nat))
  (define mul-2-real (get-f 'mul-2-real))
  (define mul-2-prob (get-f 'mul-2-prob))
  (define mul-4-prob (get-f 'mul-4-prob))

  (define e 0.00000000001)
  (check-= (c-real2prob 1.2345) (real->prob 1.2345) e)
  (check-= (c-prob2real 1.2345) (prob->real 1.2345) e)

  (check-= (recip-nat 2) 0.5 e)
  (check-= (recip-real 5.2345) (/ 1.0 5.2345) e)
  (check-= (recip-prob (c-real2prob 5.2345)) (real->prob (/ 1.0 5.2345)) e)
  (check-= (recip-prob 14.124515) (real->prob (/ 1.0 (prob->real 14.124515))) e)
  
  (check-eq? (add-2-nat 3 4) 7)
  (check-= (add-2-real 1.234 543.1234) (+ 1.234 543.1234) e)
  (check-= (add-3-real 5.324 543.2432 89.43241) (+ 5.324 543.2432 89.43241) e)
  (check-= (add-2-prob 1.234 543.1234)
           (real->prob (+ (prob->real 1.234) (prob->real 543.1234)))
           e)
  (check-= (add-3-prob 5.324 543.2432 89.43241)
           (logspace-add 5.324 543.2432 89.43241)
           e)
  (check-eq? (mul-2-nat 4 5) 20)
  (check-= (mul-2-real 4.123 5.3123) (* 4.123 5.3123) e)
  (check-= (mul-2-prob 4.123 5.3123) (+ 4.123 5.3123) e)
  (check-= (mul-2-prob 4.123 5.3123)
           (real->prob (* (prob->real 4.123) (prob->real 5.3123)))
           e)

  (define ta '(1.0 2.0 3.0 3.14 42.23))
  (define ti '(1 2 3 4 42))
  (define test-nat-array (list->cblock ti t-nat))
  (define test-real-array (list->cblock ta t-real))
  (define test-prob-array (list->cblock (map real->prob ta) t-prob))

  (define make-array-nat (get-f 'make-array<nat>))
  (define get-ptr-array-nat (get-f 'get-ptr-array<nat>))
  (define empty-array-nat (get-f 'empty-array<nat>))
  (define size-nat (get-f 'size-array<nat>))
  (define index-array-nat (get-f 'index-array<nat>))
  (define set-array-nat-at-index! (get-f 'set-index-in-array<nat>))
  (define empty-zero-array-nat (get-f 'empty-zero-array<nat>))

  (define tiarr (make-array-nat (length ti) test-nat-array))
  (define eti (empty-array-nat 5))
  (check-eq? (size-nat tiarr) 5)
  (check-eq? (index-array-nat tiarr 4) 42)
  (set-array-nat-at-index! tiarr 3 23)
  (check-eq? (index-array-nat tiarr 3) 23)

  (check-eq? (size-nat eti) 5)
  (set-array-nat-at-index! eti 3 42)
  (check-eq? (index-array-nat eti 3) 42)

  (define make-array-real (get-f 'make-array<real>))
  (define empty-array-real (get-f 'empty-array<real>))
  (define index-array-real (get-f 'index-array<real>))
  (define size-real (get-f 'size-array<real>))
  (define set-array-real-at-index! (get-f 'set-index-in-array<real>))


  (define trarr (make-array-real (length ti) test-real-array))
  (define etr (empty-array-real 5))
  (check-eq? (size-real trarr) 5)
  (check-= (index-array-real trarr 4) 42.23 e)
  (set-array-real-at-index! trarr 3 23.42)
  (check-= (index-array-real trarr 3) 23.42 e)

  (check-eq? (size-real etr) 5)
  (set-array-real-at-index! etr 3 42.23)
  (check-= (index-array-real etr 3) 42.23 e)

  (define make-array-prob (get-f 'make-array<prob>))
  (define index-array-prob (get-f 'index-array<prob>))
  (define size-prob (get-f 'size-array<prob>))
  (define set-array-prob-at-index! (get-f 'set-index-in-array<prob>))
  (define empty-array-prob (get-f 'empty-array<prob>))  

  (define tparr (make-array-prob (length ti) test-prob-array))
  (define etp (empty-array-prob 5))
  (check-eq? (size-prob tparr) 5)
  (check-= (index-array-prob tparr 4) (real->prob 42.23) e)
  (set-array-prob-at-index! tparr 3 23.42)
  (check-= (index-array-prob tparr 3) 23.42 e)

  (check-eq? (size-prob etp) 5)
  (set-array-prob-at-index! etp 3 42.23)
  (check-= (index-array-prob etp 3) 42.23 e)

  (define make-array-array-nat (get-f 'make-array<array<nat>*>))
  (define empty-array-array-nat (get-f 'empty-array<array<nat>*>))
  (define index-array-array-nat (get-f 'index-array<array<nat>*>))
  (define size-array-array-nat (get-f 'size-array<array<nat>*>))
  (define set-array-array-nat-at-index! (get-f 'set-index-in-array<array<nat>*>))


  (define etai (empty-array-array-nat 5))
  (set-array-array-nat-at-index! etai 2 eti)
  (define etib (index-array-array-nat etai 2))
  (check ptr-equal? etib eti)
  (check-eq? (index-array-nat etib 3) 42))
