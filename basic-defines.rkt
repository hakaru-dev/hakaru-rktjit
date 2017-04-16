#lang racket
(require ffi/unsafe)
(require "../racket-jit/jit.rkt")
(provide basic-defines)

(define (basic-defines)
  `((define-type nat i32)
    (define-type int i32)
    (define-type real f32)
    (define-type prob real)
    (define-type nat-p (pointer nat))
    (define-type real-p (pointer real))
    (define-type prob-p (pointer prob))
    (define-type nat-pp (pointer nat-p))
    (define-type real-pp (pointer real-p))
    (define-type prob-pp (pointer prob-p))
    (define-type array-real (struct (size : i32) (data : real-p)))
    (define-type array-real-p (pointer array-real))
    (define-type array-prob (struct (size : i32) (data : prob-p)))
    (define-type array-prob-p (pointer array-prob))

    (define-type array-nat (struct (size : i32) (data : nat-p)))
    (define-type array-nat-p (pointer array-nat))
    (define-function (prob2real (v : real) : real) (return v))
    (define-function (nat2prob (v : nat) : real) (return (#%app jit-ui->fp v (#%type real))))
    
    ,@(append*
       (for/list ([type '(nat real prob)])
         (let ((array-type (string->symbol (format "array-~a" type)))
               (array-type-p (string->symbol (format "array-~a-p" type)))
               (type-p (string->symbol (format "~a-p" type)))
               (type-pp (string->symbol (format "~a-pp" type))))
           `((define-function
               (,(string->symbol (format "make-array-~a" type))
                (size : i32)
                (data : ,type-p)
                : ,array-type-p)
               (let ((ap : ,array-type-p (#%app jit-malloc (#%type ,array-type)))
                     (ap-size* : nat-p (#%gep ap ((#%value 0 nat) (#%value 0 nat))))
                     (ap-data* : ,type-pp (#%gep ap ((#%value 0 nat) (#%value 1 nat)))))
                 (block
                  (#%exp (#%app jit-store! size ap-size*))
                  (#%exp (#%app jit-store! data ap-data*))
                  (return ap))))
             (define-function
               (,(string->symbol (format "get-array-~a" type))
                (s : ,array-type-p)
                : ,type-p)
               (let ((atp : ,type-p (#%gep s ((#%value 0 nat) (#%value 1 nat)))))
                 (return (#%app jit-load atp))))
             (define-function
               (,(string->symbol (format "empty-array-~a" type))
                (size : i32)
                : ,array-type-p)
               (let ((ap : ,array-type-p (#%app jit-malloc (#%type ,array-type)))
                     (data : ,type-p (#%app jit-arr-malloc (#%type ,type) size))
                     (atp : ,type-pp (#%gep ap ((#%value 0 nat) (#%value 1 nat))))
                     (sizep : nat-p (#%gep ap ((#%value 0 nat) (#%value 0 nat)))))
                 (block
                  (#%exp (#%app jit-store! size sizep))
                  (#%exp (#%app jit-store! data atp))
                  (return ap))))
             (define-function (,(string->symbol (format "size-~a" array-type))
                               (array-ptr : ,array-type-p) : i32)
               (return (#%app jit-load (#%gep array-ptr ((#%value 0 int) (#%value 0 int))))))

             (define-function (,(string->symbol (format "index-~a" array-type))
                               (array-ptr : ,array-type-p) (index : i32) : ,type)
               (return (#%app jit-load
                              (#%gep
                               (#%app jit-load
                                      (#%gep array-ptr
                                             ((#%value 0 int) (#%value 1 int))))
                               (index)))))
             (define-function (,(string->symbol (format "recip-~a" type)) (v : ,type) : real)
               (return (#%app jit-fdiv
                              (#%fl-value 1.0 real)
                              (#%app jit-ui->fp v (#%type real)))))
             (define-function (,(string->symbol (format "set-array-~a-at-index" type))
                               (arr : ,array-type-p) (in : nat) (v : ,type) : void)
               (block (#%exp (#%app jit-store! v (#%gep
                                            (#%app jit-load
                                                   (#%gep arr
                                                          ((#%value 0 int) (#%value 1 int))))
                                            (in))))
                     (return-void)))))))))

(module+ test
  (require rackunit)
  (pretty-display (basic-defines))
  (define benv  (initialize-jit (compile-module `(#%module ,@(basic-defines)))))
  (jit-dump-module benv)
  (define (get-f f) (jit-get-function f benv))
  
  (define prob2real (get-f 'prob2real))
  (define nat2prob (get-f 'nat2prob))
  (define recip-real (get-f 'recip-real))
  (define recip-nat (get-f 'recip-nat))

  (define r-real (jit-get-racket-type (env-lookup 'real benv)))
  (define r-nat (jit-get-racket-type (env-lookup 'nat benv)))
  (define test-real-array (list->cblock '(1.0 2.0 3.0 3.14 42.23) r-real))


  (define make-array-real (get-f 'make-array-real))
  (define index-array-real (get-f 'index-array-real))
  (define size-real (get-f 'size-array-real))
  (define set-array-real-at-index! (get-f 'set-array-real-at-index!))

  (define empty-array-nat (get-f 'empty-array-nat))
  (define make-array-nat (get-f 'make-array-nat))
  (define index-array-nat (get-f 'index-array-nat))
  (define size-nat (get-f 'size-array-nat))
  
  (define tarr (make-array-real 5 test-real-array))
  (set-array-real-at-index! tarr 0 4.2)
  (check-eq? (size-real tarr) 5)
  ;; (check-eq? (index-array-real tarr 0) 4.2)
  (printf "index-array-real: 4.2 == ~a\n" (index-array-real tarr 0))
  (define test-nat-array (list->cblock '(1 2 3 4 42) r-nat))
  (define tnat (make-array-nat 5 test-nat-array))
  (check-eq? (size-nat tnat) 5)
  (check-eq? (index-array-nat tnat 4) 42)

  (printf "prob2real: ~a\n" (prob2real 1.5))
  (printf "nat2prob: ~a\n" (nat2prob 3))
  (printf "recip: ~a\n" (recip-real 2.0))
  (printf "recip: ~a\n" (recip-nat 2))
  )
