#lang racket
(require ffi/unsafe)
(require sham/jit)
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

(define (basic-defines)
  `((define-type nat i32)
    (define-type int i32)
    (define-type data-type i32)
    (define-type real f64)
    (define-type prob real)

    (define-type natm (pointer prob)) ;;TODO for measure

    (define-type nat-p (pointer nat))
    (define-type nat-pp (pointer nat-p))
    (define-type nat-ppp (pointer nat-pp))


    (define-type real-p (pointer real))
    (define-type real-pp (pointer real-p))
    (define-type real-ppp (pointer real-pp))

    (define-type prob-p (pointer prob))
    (define-type prob-pp (pointer prob-p))
    (define-type prob-ppp (pointer prob-pp))

    (define-type array-real (struct (size : i32) (data : real-p)))
    (define-type array-real-p (pointer array-real))
    (define-type array-prob (struct (size : i32) (data : prob-p)))
    (define-type array-prob-p (pointer array-prob))

    (define-type array-nat (struct (size : i32) (data : nat-p)))
    (define-type array-nat-p (pointer array-nat))
    (define-type array-nat-pp (pointer array-nat-p))
    (define-type array-array-nat (struct (size : i32) (data : array-nat-p)))
    (define-type array-array-nat-p (pointer array-array-nat))
    (define-function (#:attr AlwaysInline)
      (categorical (arr : prob-p) : natm)
      (return arr))



    (define-function (#:attr AlwaysInline)
      (nat2prob (v : nat) : prob)
      (return (#%app real2prob
                     (#%app jit-ui->fp v (#%type real)))))
    (define-function
      (#:attr AlwaysInline)
      (prob2real
       (v : prob) : real)
      (return (#%app (#%jit-intr llvm.exp.f64 prob) v)))
    (define-function
      (#:attr AlwaysInline)
      (real2prob
       (v : real) : prob)
      (return (#%app (#%jit-intr llvm.log.f64 prob) v)))

    (define-function
      (#:attr AlwaysInline)
      (recip-nat
       (v : nat) : real)
      (return (#%app jit-fdiv
                     (#%fl-value 1.0 real)
                     (#%app jit-ui->fp v (#%type real)))))
    (define-function
      (#:attr AlwaysInline)
      (recip-real
       (v : real) : real)
      (return (#%app jit-fdiv (#%fl-value 1.0 real) v)))
    (define-function
      (#:attr AlwaysInline)
      (recip-prob
       (v : real) : real)
      (return (#%app jit-fmul (#%fl-value -1.0 real) v)))
    (define-function
      (#:attr AlwaysInline)
      (add-2-nat
       (v1 : nat) (v2 : nat) : nat)
      (return (#%app jit-add-nuw v1 v2)))
    (define-function
      (#:attr AlwaysInline)
      (add-2-real
       (v1 : real) (v2 : real) : real)
      (return (#%app jit-fadd v1 v2)))
    (define-function
      (#:attr AlwaysInline)
      (add-3-real
       (v1 : real) (v2 : real) (v3 : real) : real)
      (return (#%app jit-fadd (#%app jit-fadd v1 v2) v3)))

    (define-function
      (#:attr AlwaysInline)
      (add-2-prob
       (v1 : prob) (v2 : prob) : prob)
      (return (#%app real2prob
                     (#%app add-2-real
                      (#%app prob2real v1)
                      (#%app prob2real v2)))))




    (define-function
      (#:attr AlwaysInline)
      (add-3-prob
       (v1 : prob) (v2 : prob) (v3 : prob) : prob)
      (return (#%app real2prob
               (#%app add-3-real
                      (#%app prob2real v1)
                      (#%app prob2real v2)
                      (#%app prob2real v3)))))
    (define-function
      (#:attr AlwaysInline)
      (mul-2-nat
       (v1 : nat) (v2 : nat) : nat)
      (return (#%app jit-mul-nuw v1 v2)))
    (define-function
      (#:attr AlwaysInline)
      (mul-2-real
       (v1 : real) (v2 : real) : real)
      (return (#%app jit-fmul v1 v2)))
    (define-function
      (#:attr AlwaysInline)
      (mul-2-prob
       (v1 : prob) (v2 : prob) : prob)
      (return (#%app jit-fadd v1 v2)))
    (define-function
      (#:attr AlwaysInline)
      (mul-4-prob
       (v1 : prob) (v2 : prob) (v3 : prob) (v4 : prob) : prob)
      (return (#%app jit-fadd v4 (#%app jit-fadd v3 (#%app jit-fadd v1 v2)))))

    ;; ,(get-array-type 'nat-p 'nat-pp 'nat-ppp 'array-nat-p 'array-nat-pp)
    ,@(append*
       (for/list ([type '(nat real prob array-nat)])
         (let ((array-type (string->symbol (format "array-~a" type)))
               (array-type-p (string->symbol (format "array-~a-p" type)))
               (type-p (string->symbol (format "~a-p" type)))
               (type-pp (string->symbol (format "~a-pp" type))))
           (array-functions type type-p type-pp array-type array-type-p))))))

(module+ test
  (require rackunit)
  (require "utils.rkt")
  (pretty-display (basic-defines))
  (define benv  (initialize-jit (compile-module `(#%module ,@(basic-defines)))))
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
