#lang racket

(require sham/ast
         "type-defines.rkt"
         "basic-defines.rkt"
         "array-defines.rkt"
         "pair-defines.rkt"
         "probability-defines.rkt")

(provide (all-defined-out))

(define (new-prelude)
  (box '()))

(define (add-defs-prelude! prelude defs)
  (set-box! prelude (append (unbox prelude) defs))
  prelude)

(define (get-defs-prelude prelude)
  (unbox prelude))

(define (add-basic&probability-defs prl)
  (add-defs-prelude! prl (basic-defs))
  (add-defs-prelude! prl (probability-defs)))

(define (cleanup-defs defs)
  (define already-seen (mutable-set))
  (for/list ([def defs]
             #:when (not (set-member? already-seen (sham:def-id def))))
    (set-add! already-seen (sham:def-id def))
    def))

;;returns a list of all defines needed for the type
(define (get-all-defs-type type-ast)
  (match type-ast
    [`(array ,arr-t)
     (append (array-defs type-ast) (get-all-defs-type arr-t))]
    [`(pair ,ta ,tb)
     (append (pair-defs type-ast)
             (get-all-defs-type ta)
             (get-all-defs-type tb))]
    ['nat (list type-nat-def)]
    ['int (list type-nat-def)]
    ['prob (list type-prob-def)]
    ['bool  (list type-bool-def)]
    ['real (list type-real-def)]
    ['unit (list type-nat-def)]
    ['void (list type-void-def)]
    [else (error "unknown type" type-ast)]))

(define (get-type-ref type-def)
  (get-sham-type-ref type-def))

(define (get-rator sym tresult trands)
  ((match sym
     [basic-rator? get-basic-rator]
     [pair-rator? get-pair-rator]
     [array-rator? get-array-rator]
     [probability-rator? get-probability-rator]
     [else (error "why is this rator not done yet?" sym tresult trands)])
   sym tresult trands))

(define (get-value v type)
  (match type
    ['nat (nat-value (truncate (inexact->exact v)))]
    ['unit (nat-value (truncate (inexact->exact 0)))]
    ['prob (sham:expr:app (sham:rator:symbol'real2prob)
                          (list (real-value (exact->inexact v))))]
    ['real (real-value (exact->inexact v))]
    ['int (sham:expr:var 'figuroutint)]))

(module+ test
  (map print-sham-def
     (get-all-defs-type
      '(array (pair (array prob) (pair real prob))))))
