#lang racket

(require sham/private/ast)
(require "basic-defines.rkt")

(provide add-fluff)

(define (add-fluff m)
  (match m
    [(sham:module p fns)
     (sham:module '((passes . (AlwaysInliner LoopVectorize))
                    (ffi-libs . ((libgslcblas . ("libgslcblas" #:global? #t))
                                 (libgsl . ("libgsl")))))
                  (append (basic-defines) fns))]))
