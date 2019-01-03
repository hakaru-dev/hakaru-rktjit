#lang racket
(require
 sham/private/ast-utils
 "../utils.rkt")

(provide (all-defined-out)
         (all-from-out "../utils.rkt"))

(define (get-struct-field var . idx)
  (gep var (cons (ui32 0) (map ui32 idx))))
