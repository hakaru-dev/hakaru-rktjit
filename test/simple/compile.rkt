#lang racket

(require sham
         sham/jit-utils
         hakrit/jit
         hakrit/utils)

(define fname "array-product.hkr")
(define input-dir "./")

(define a (make-sized-hakrit-array (list 0.0 0.0 0.0) 'real))
(define module-env (debug-file (build-path input-dir fname) '()))

(define prog (get-prog module-env))
(define result (prog a))
