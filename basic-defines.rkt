#lang racket
(require "../libjit/jit.rkt")

(define (basic-defines)
  `((define-type nat uint)
    (define-type real float32)
    (define-type nat-p (pointer nat))
    (define-type real-p (pointer real))
    (define-type array-real (struct (size : int) (data : real-p)))
    (define-type array-real-p (pointer array-real))
    (define-type array-nat (struct (size : int) (data : nat-p)))
    (define-type array-nat-p (pointer array-nat))
    (define-function (prob2real (v : real) : real) (return v))
    (define-function (nat2prob (v : nat) : real) (return v))
    (define-function (recip (v : real) : real)
      (return (#%app jit-div
                     (#%value 1 int)
                     v)))
    ,@(append*
       (for/list ([type '(nat real)])
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
             (define-function (,(string->symbol (format "size-~a" array-type))
                               (array-ptr : ,array-type-p) : int)
               (return (* array-ptr (#%offset ,array-type size) : int)))
             (define-function (,(string->symbol (format "index-~a" array-type))
                               (array-ptr : ,array-type-p) (index : int) : ,type)
               (return (* array-ptr
                          (#%app jit-add
                                 (#%offset ,array-type data)
                                 (#%app jit-mul (#%sizeof ,type) index))
                          : ,type)))))))
    ))

(module+ test
  (pretty-display (basic-defines))
  (define benv  (compile-module `(module ,@(basic-defines)))))
