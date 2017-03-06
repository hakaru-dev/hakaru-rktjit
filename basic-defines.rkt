#lang racket
(require ffi/unsafe)
(require "../libjit/jit.rkt")
(provide basic-defines)

(define (basic-defines)
  `((define-type nat uint)
    (define-type real float32)
    (define-type prob real)
    (define-type nat-p (pointer nat))
    (define-type real-p (pointer real))
    (define-type prob-p (pointer prob))
    (define-type array-real (struct (size : int) (data : real-p)))
    (define-type array-real-p (pointer array-real))
    (define-type array-prob (struct (size : int) (data : prob-p)))
    (define-type array-prob-p (pointer array-prob))

    (define-type array-nat (struct (size : int) (data : nat-p)))
    (define-type array-nat-p (pointer array-nat))
    (define-function (prob2real (v : real) : real) (return v))
    (define-function (nat2prob (v : nat) : real) (return v))
    
    ,@(append*
       (for/list ([type '(nat real prob)])
         (let ((array-type (string->symbol (format "array-~a" type)))
               (array-type-p (string->symbol (format "array-~a-p" type)))
               (type-p (string->symbol (format "~a-p" type))))
           `((define-function
               (,(string->symbol (format "make-array-~a" type))
                (size : int)
                (data : ,type-p)
                : ,array-type-p)
               (let ((ap : ,array-type-p))
                 (block
                  (set! ap (#%app jit-malloc (#%sizeof ,array-type)))
                  (set! (* ap (#%offset ,array-type size) : int) size)
                  (set! (* ap (#%offset ,array-type data) : ,type-p) data)
                  (return ap))))
             (define-function
               (,(string->symbol (format "get-array-~a" type))
                (s : ,array-type-p)
                : ,type-p)
               (return (* s (#%offset ,array-type data) : ,type-p)))
             (define-function
               (,(string->symbol (format "empty-~a-array" type))
                (size : int)
                : ,array-type-p)
               (let ((ap : ,array-type-p)
                     (data : ,type-p))
                 (block
                  (set! data (#%app jit-malloc (#%app jit-mul (#%sizeof ,type) size)))
                  (set! ap (#%app jit-malloc (#%sizeof ,array-type)))
                  (set! (* ap (#%offset ,array-type size) : int) size)
                  (set! (* ap (#%offset ,array-type data) : ,type-p) data)
                  (return ap))))
             (define-function (,(string->symbol (format "size-~a" array-type))
                               (array-ptr : ,array-type-p) : int)
               (return (* array-ptr (#%offset ,array-type size) : int)))
             (define-function (,(string->symbol (format "index-~a" array-type))
                               (array-ptr : ,array-type-p) (index : int) : ,type)
               (let ((datap : ,type-p))
                 (block
                  (set! datap (* array-ptr (#%offset ,array-type data) : ,type-p))
                  (return (* datap (#%app jit-mul (#%sizeof ,type) index) : ,type)))))
             (define-function (,(string->symbol (format "recip-~a" type)) (v : ,type) : real)
               (return (#%app jit-div
                              (#%value 1.0 real)
                              v)))
             (define-function (,(string->symbol (format "set-array-~a-at-index!" type))
                               (arr : ,array-type-p) (in : nat) (v : ,type) : void)
               (let ((datap : ,type-p))
                 (block
                  (set! datap (* arr (#%offset ,array-type data) : ,type-p))
                  (set! (* datap (#%app jit-mul (#%sizeof ,type) in) : ,type) v))))))))))

(module+ test
  (pretty-display (basic-defines))
  (define benv  (compile-module `(module ,@(basic-defines))))
  (define (get-f fname) (jit-get-function (env-lookup fname benv)))

  (define prob2real (get-f 'prob2real))
  (define nat2prob (get-f 'nat2prob))
  (define recip-real (get-f 'recip-real))
  (define recip-nat (get-f 'recip-nat))

  (define r-real (jit-get-racket-type (env-lookup 'real benv)))
  (define r-nat (jit-get-racket-type (env-lookup 'nat benv)))
  (define test-real-array (list->cblock '(1.0 2.0 3.0 3.14 42.23) r-real))
  (define test-nat-array (list->cblock '(1 2 3 4 42) r-nat))

  (define make-array-real (get-f 'make-array-real))
  (define index-array-real (get-f 'index-array-real))
  (define size-real (get-f 'size-array-real))
  (define set-array-real-at-index! (get-f 'set-array-real-at-index!))

  (define make-array-nat (get-f 'make-array-nat))
  (define index-array-nat (get-f 'index-array-nat))
  (define size-nat (get-f 'size-array-nat))
  
  (define tarr (make-array-real 5 test-real-array))
  (set-array-real-at-index! tarr 0 3.0)
  (printf "real array, size: ~a, [0]: ~a, [1]: ~a,\n"
           (size-real tarr)
           (index-array-real tarr 0)
           (index-array-real tarr 1))

  (define tnat (make-array-nat 5 test-nat-array))
  (printf "nat array, size: ~a, [0]: ~a, [1]: ~a,\n"
           (size-nat tnat)
           (index-array-nat tnat 0)
           (index-array-nat tnat 1))

  (printf "prob2real: ~a\n" (prob2real 1.5))
  (printf "nat2prob: ~a\n" (nat2prob 3))
  (printf "recip: ~a\n" (recip-real 2.0))
  (printf "recip: ~a\n" (recip-nat 2)))
