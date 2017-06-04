#lang racket
(require ffi/unsafe)
(require sham/jit)

(require ffi/unsafe)
(require (for-syntax racket/syntax))
(provide hakaru-defines)
(define-syntax (hakaru-defines stx)
  (syntax-case stx ()
    [(_ me)
     #`(begin
         #,@(for/list ([t '(nat prob real)])
              (with-syntax ([ts (format-id stx "~a" t)]
                            [type (format-id stx "~a-type" t)]
                            [make-array (format-id stx "make-array-~a" t)]
                            [make-c-array (format-id stx "make-c-array-~a" t)]
                            [mac-v (format-id stx "~a-vector" t)]
                            [get-c-array (format-id stx "get-array-~a" t)]
                            [size-array (format-id stx "size-array-~a" t)]
                            [size-array-p (format-id stx "size-array-~a-p" t)]
                            [get-vector (format-id stx "get-~a-vector" t)])
                #`(begin
                    (define type (jit-get-racket-type (env-lookup (quote ts) me)))
                    (define make-array (jit-get-function (quote make-array) me))
                    (define (make-c-array mac-v)
                      (make-array (vector-length mac-v)
                                  (vector->cblock mac-v type)))
                    (define get-c-array (jit-get-function (quote get-c-array) me))
                    (define size-array (jit-get-function (quote size-array-p) me))
                    (define (get-vector cs)
                      (cblock->vector (get-c-array cs) type (size-array cs)))))))]))
