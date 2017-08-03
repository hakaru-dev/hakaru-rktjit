#lang racket
(require ffi/unsafe)
(require sham/jit)
(require sham/private/ast)
(provide basic-defines)

(define (make-array-type type type-p type-pp array-type array-type-p)
  (define fn-name (string->symbol (format "make-array-~a" type)))
  `(define-function
     (#:attr AlwaysInline)
     (,fn-name
      (size : i32)
      (data : ,type-p)
      : ,array-type-p)
     (let ((ap : ,array-type-p (#%app jit-malloc (#%type ,array-type)))
           (ap-size* : nat-p (#%gep ap ((#%ui-value 0 nat) (#%ui-value 0 nat))))
           (ap-data* : ,type-pp (#%gep ap ((#%ui-value 0 nat) (#%ui-value 1 nat)))))
       (block
        (#%exp (#%app jit-store! size ap-size*))
        (#%exp (#%app jit-store! data ap-data*))
        (return ap)))))
(define (get-array-type type type-p type-pp array-type array-type-p)
  `(define-function
     (#:attr AlwaysInline)
     (,(string->symbol (format "get-array-~a" type))
      (s : ,array-type-p)
      : ,type-p)
     (let ((atp : ,type-p (#%gep s ((#%ui-value 0 nat) (#%ui-value 1 nat)))))
       (return (#%app jit-load atp)))))
(define (empty-array-type type type-p type-pp array-type array-type-p)
  `(define-function
     (#:attr AlwaysInline)
     (,(string->symbol (format "empty-array-~a" type))
      (size : i32)
      : ,array-type-p)
     (let ((ap : ,array-type-p (#%app jit-malloc (#%type ,array-type)))
           (data : ,type-p (#%app jit-arr-malloc (#%type ,type) size))
           (atp : ,type-pp (#%gep ap ((#%ui-value 0 nat) (#%ui-value 1 nat))))
           (sizep : nat-p (#%gep ap ((#%ui-value 0 nat) (#%ui-value 0 nat)))))
       (block
        (#%exp (#%app jit-store! size sizep))
        (#%exp (#%app jit-store! data atp))
        (return ap)))))
(define (empty-array-type-zero type type-p type-pp array-type array-type-p)
  `(define-function
     (#:attr AlwaysInline)
     (,(string->symbol (format "empty-array-~a-zero" type))
      : ,array-type-p)
     (return (#%app ,(string->symbol (format "empty-array-~a" type)) (#%ui-value 0 nat)))))
(define (size-array-type type type-p type-pp array-type array-type-p)
  `(define-function
     (#:attr AlwaysInline)
     (,(string->symbol (format "size-~a-p" array-type))
      (array-ptr : ,array-type-p) : i32)
     (return (#%app jit-load (#%gep array-ptr ((#%ui-value 0 nat) (#%ui-value 0 nat)))))))
(define (index-array-type type type-p type-pp array-type array-type-p)
  `(define-function
     (#:attr AlwaysInline)
     (,(string->symbol (format "index-~a-p" array-type))
      (array-ptr : ,array-type-p) (index : i32) : ,type)
     (return (#%app jit-load
                    (#%gep
                     (#%app jit-load
                            (#%gep array-ptr
                                   ((#%ui-value 0 nat) (#%ui-value 1 nat))))
                     (index))))))
(define (set-array-type-at-index type type-p type-pp array-type array-type-p)
  `(define-function
     (#:attr AlwaysInline)
     (,(string->symbol (format "set-array-~a-at-index" type))
      (arr : ,array-type-p) (in : nat) (v : ,type) : void)
     (block (#%exp (#%app jit-store! v
                          (#%gep
                           (#%app jit-load
                                  (#%gep arr
                                         ((#%ui-value 0 nat) (#%ui-value 1 nat))))
                           (in))))
            (return-void))))

(define (array-functions type type-p type-pp array-type array-type-p)
  `(,(make-array-type type type-p type-pp array-type array-type-p)
    ,(get-array-type type type-p type-pp array-type array-type-p)
    ,(empty-array-type type type-p type-pp array-type array-type-p)
    ,(empty-array-type-zero type type-p type-pp array-type array-type-p)
    ,(size-array-type type type-p type-pp array-type array-type-p)
    ,(index-array-type type type-p type-pp array-type array-type-p)
    ,(set-array-type-at-index type type-p type-pp array-type array-type-p)))

(require (for-syntax racket/syntax))
(define (basic-defines)
  (define i32 (sham:type:ref 'i32))
  (define f64 (sham:type:ref 'f64))
  (define nat i32)
  (define int i32)
  (define real f64)
  (define prob f64)
  (define-syntax (create-ptr-def stx)
    (syntax-case stx ()
      [(_ ptr)
       (with-syntax ([ptr* (format-id stx "~a*"  #'ptr)])
         #'(sham:def:type 'ptr* (sham:type:pointer (sham:type:ref 'ptr))))]))
  (define-syntax (create-ptr-defs stx)
    (syntax-case stx ()
      [(_ ptr ...)
       #'(list (create-ptr-def ptr) ...)]))

  (define-syntax (create-array-def stx)
    (syntax-case stx ()
      [(_ type)
       (with-syntax ([type* (format-id stx "~a*"  #'type)]
                     [atype (format-id stx "array<~a>" #'type)])
         #'(sham:def:type 'atype (sham:type:struct
                                  '(size data)
                                  (list nat (sham:type:ref 'type*)))))]))
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

  (define (real-value v)
    (sham:exp:fl-value v (sham:type:ref 'real)))
  (define sham$var sham:exp:var)
  (define-syntax (sham$app stx)
    (syntax-case stx ()
      [(_ (str t) rands ...)
       #'(sham:exp:app (sham:rator:intrinsic 'str
                                             (sham:type:ref 't))
                     (list rands ...))]
      [(_ sym rands ...)
       #'(sham:exp:app (sham:rator:symbol 'sym) (list rands ...))]))
  (define-syntax (sham$app-var stx)
    (syntax-case stx ()
      [(_ app rands ...)
       #'(sham$app app (sham:exp:var 'rands) ...)]))
  (define-syntax (sham$define stx)
    (syntax-case stx (return :)
      [(_ (id (args : t) ... : rett) (return stmt))
       #'(sham:def:function
          'id '() '(AlwaysInline)
          '(args ...) (list (sham:type:ref 't) ...) (sham:type:ref 'rett)
          (sham:stmt:return stmt))]))
  
  (define simple-funs
    (list
    ;; (define-function (#:attr AlwaysInline) ;;TODO
    ;;   (categorical (arr : prob-p) : natm)
    ;;   (return arr))

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
                        (sham$app fadd (sham$var 'v3) (sham$app-var fadd v1 v2)))))

    ;; (sham$define
    ;;   (make-array-array-nat (size : i32) (data : array-nat-pp) : array-array-nat-p)
    ;;   (let ((ap : array-array-nat-p (#%app jit-malloc (#%type array-array-nat)))
    ;;         (ap-size* : nat-p (#%gep ap ((#%ui-value 0 nat) (#%ui-value 0 nat))))
    ;;         (ap-data*
    ;;          :
    ;;          array-nat-ppp
    ;;          (#%gep ap ((#%ui-value 0 nat) (#%ui-value 1 nat)))))
    ;;     (block
    ;;      (#%exp (#%app jit-store! size ap-size*))
    ;;      (#%exp (#%app jit-store! data ap-data*))
    ;;      (return ap))))
    ;; (define-function
    ;;   (#:attr AlwaysInline)
    ;;   (get-array-array-nat (s : array-array-nat-p) : array-nat-pp)
    ;;   (let ((atp : array-nat-p (#%gep s ((#%ui-value 0 nat) (#%ui-value 1 nat)))))
    ;;     (return (#%app jit-load atp))))
    ;; (define-function
    ;;   (#:attr AlwaysInline)
    ;;   (empty-array-array-nat (size : i32) : array-array-nat-p)
    ;;   (let ((ap : array-array-nat-p (#%app jit-malloc (#%type array-array-nat)))
    ;;         (data : array-nat-pp (#%app jit-arr-malloc (#%type array-nat-p) size))
    ;;         (atp : array-nat-ppp (#%gep ap ((#%ui-value 0 nat) (#%ui-value 1 nat))))
    ;;         (sizep : nat-p (#%gep ap ((#%ui-value 0 nat) (#%ui-value 0 nat)))))
    ;;     (block
    ;;      (#%exp (#%app jit-store! size sizep))
    ;;      (#%exp (#%app jit-store! data atp))
    ;;      (return ap))))
    ;; (define-function
    ;;   (#:attr AlwaysInline)
    ;;   (empty-array-array-nat-zero : array-array-nat-p)
    ;;   (return (#%app empty-array-array-nat (#%ui-value 0 nat))))
    ;; (define-function
    ;;   (#:attr AlwaysInline)
    ;;   (size-array-array-nat-p (array-ptr : array-array-nat-p) : i32)
    ;;   (return
    ;;    (#%app jit-load (#%gep array-ptr ((#%ui-value 0 nat) (#%ui-value 0 nat))))))
    ;; (define-function
    ;;   (#:attr AlwaysInline)
    ;;   (index-array-array-nat-p
    ;;    (array-ptr : array-array-nat-p)
    ;;    (index : i32)
    ;;    :
    ;;    array-nat-p)
    ;;   (return
    ;;    (#%app jit-load
    ;;           (#%gep
    ;;            (#%app jit-load (#%gep array-ptr ((#%ui-value 0 nat) (#%ui-value 1 nat))))
    ;;            (index)))))
    ;; (define-function
    ;;   (#:attr AlwaysInline)
    ;;   (set-array-array-nat-at-index
    ;;    (arr : array-array-nat-p)
    ;;    (in : nat)
    ;;    (v : array-nat-p)
    ;;    :
    ;;    void)
    ;;   (block
    ;;    (#%exp
    ;;     (#%app
    ;;      jit-store!
    ;;      v
    ;;      (#%gep
    ;;       (#%app jit-load (#%gep arr ((#%ui-value 0 nat) (#%ui-value 1 nat))))
    ;;       (in))))
    ;;    (return-void)))


    ;; ,@(append*
    ;;    (for/list ([type '(nat real prob)])
    ;;      (let ((array-type (string->symbol (format "array-~a" type)))
    ;;            (array-type-p (string->symbol (format "array-~a-p" type)))
    ;;            (type-p (string->symbol (format "~a-p" type)))
    ;;            (type-pp (string->symbol (format "~a-pp" type))))
    ;;        (array-functions type type-p type-pp array-type array-type-p))))
    ))
  (append types simple-funs))


(module+ test
  (require rackunit)
  (require "utils.rkt")
  ;(pretty-display (basic-defines))
  (define
    benv
    (initialize-jit
     (compile-module
      (sham:module '()
                   (basic-defines)))))
  
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

  (define make-array-nat (get-f 'make-array-nat))
  (define index-array-nat (get-f 'index-array-nat-p))
  (define size-nat (get-f 'size-array-nat-p))
  (define set-array-nat-at-index! (get-f 'set-array-nat-at-index))
  (define empty-array-nat (get-f 'empty-array-nat))

  (define tiarr (make-array-nat (length ti) test-nat-array))
  (define eti (empty-array-nat 5))
  (check-eq? (size-nat tiarr) 5)
  (check-eq? (index-array-nat tiarr 4) 42)
  (set-array-nat-at-index! tiarr 3 23)
  (check-eq? (index-array-nat tiarr 3) 23)

  (check-eq? (size-nat eti) 5)
  (set-array-nat-at-index! eti 3 42)
  (check-eq? (index-array-nat eti 3) 42)

  (define make-array-array-nat (get-f 'make-array-array-nat))
  (define index-array-array-nat (get-f 'index-array-array-nat-p))
  (define size-array-array-nat (get-f 'size-array-array-nat-p))
  (define set-array-array-nat-at-index! (get-f 'set-array-array-nat-at-index))
  (define empty-array-array-nat (get-f 'empty-array-array-nat))

  (define etai (empty-array-array-nat 5))
  (set-array-array-nat-at-index! etai 2 eti)
  (define etib (index-array-array-nat etai 2))
  (check ptr-equal? etib eti)
  (check-eq? (index-array-nat etib 3) 42)



  (define make-array-real (get-f 'make-array-real))
  (define index-array-real (get-f 'index-array-real-p))
  (define size-real (get-f 'size-array-real-p))
  (define set-array-real-at-index! (get-f 'set-array-real-at-index))
  (define empty-array-real (get-f 'empty-array-real))

  (define trarr (make-array-real (length ti) test-real-array))
  (define etr (empty-array-real 5))
  (check-eq? (size-real trarr) 5)
  (check-= (index-array-real trarr 4) 42.23 e)
  (set-array-real-at-index! trarr 3 23.42)
  (check-= (index-array-real trarr 3) 23.42 e)

  (check-eq? (size-real etr) 5)
  (set-array-real-at-index! etr 3 42.23)
  (check-= (index-array-real etr 3) 42.23 e)

  (define make-array-prob (get-f 'make-array-prob))
  (define index-array-prob (get-f 'index-array-prob-p))
  (define size-prob (get-f 'size-array-prob-p))
  (define set-array-prob-at-index! (get-f 'set-array-prob-at-index))
  (define empty-array-prob (get-f 'empty-array-prob))  

  (define tparr (make-array-prob (length ti) test-prob-array))
  (define etp (empty-array-prob 5))
  (check-eq? (size-prob tparr) 5)
  (check-= (index-array-prob tparr 4) (real->prob 42.23) e)
  (set-array-prob-at-index! tparr 3 23.42)
  (check-= (index-array-prob tparr 3) 23.42 e)

  (check-eq? (size-prob etp) 5)
  (set-array-prob-at-index! etp 3 42.23)
  (check-= (index-array-prob etp 3) 42.23 e))
