#lang racket

(require sham/ast
         "type-defines.rkt"
         "basic-defines.rkt"
         "array-defines.rkt"
         "pair-defines.rkt"
         "struct-defines.rkt"
         "probability-defines.rkt")

(provide (all-defined-out)
         basic-mod-info
         prog-fun-info)

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

;;returns (values type-ref (list type-defs ...))
(define (get-defs&ref-type type-ast)
  (define tast (if-need-pointer type-ast))
  (values
   (get-sham-type-ref-ast tast)
   (append
    (match type-ast
      [`(array ,arr-t)
       (define-values (_ odefs) (get-defs&ref-type arr-t))
       (append (array-defs type-ast) odefs)]
      [`(array ,arr-t (size . ,t))
       (define-values (_ odefs)
         (get-defs&ref-type arr-t))
       (append (const-array-defs type-ast) odefs)]
      [`(struct-type (,ts ...))
       (define tsdefs (map (λ (t) (define-values (_ odefs) (get-defs&ref-type t))  odefs) ts))
       (append (get-struct-defs type-ast) tsdefs)]
      [`(pair ,ta ,tb)
       (define-values (taref tadefs) (get-defs&ref-type ta))
       (define-values (tbref tbdefs) (get-defs&ref-type tb))
       (append (pair-defs type-ast)
               tadefs
               tbdefs)]
      [`(measure ,t) (define-values (_ defs) (get-defs&ref-type t)) defs]
      ['nat (list type-nat-def)]
      ['int (list type-nat-def)]
      ['prob (list type-prob-def)]
      ['bool  (list type-bool-def)]
      ['real (list type-real-def)]
      ['unit (list type-nat-def)]
      ['void (list type-void-def)]
      [`(,t ,_) #:when (nrp? t) (define-values (ref def) (get-defs&ref-type t)) def]
      [else (error "unknown type" type-ast)])
    (list (car (get-sham-type-define tast))))))


(define (get-type-ref type-def)
  (get-sham-type-ref type-def))

(define (get-rator sym tresult trands)
  ((match sym
     [(? basic-rator?) get-basic-rator]
     [(? pair-rator?) get-pair-rator]
     [(? array-rator?) get-array-rator]
     [(? probability-rator?) get-probability-rator]
     [(? struct-rator?) get-struct-rator]
     [else (error "why is this rator not done yet?" sym tresult trands)])
   sym (remove-measure tresult) (map remove-measure trands)))

(define (get-value v type)
  (match (clean-type-info type)
    ['nat (nat-value (truncate (inexact->exact v)))]
    ['unit (nat-value (truncate (inexact->exact 0)))]
    ['prob (prob-value (exact->inexact v))]
    ['real (real-value (exact->inexact v))]
    ['int (sham:expr:si-value v type-int-ref)]
    ['bool (sham:expr:ui-value v type-bool-ref)]
    [`(measure ,t) (get-value v t)]))

(define (clean-type-info t)
  (match t
    [`(nat ,_) 'nat]
    [`(real ,_) 'real]
    [`(prob ,_) 'prob]
    [`(int ,_) 'int]
    [`(array ,t ,_) `(array ,t)]
    [else t]))
(module+ test
  (define-values
    (ref defs)
    (get-defs&ref-type
     '(array (pair (array prob) (pair real prob)))))
  (printf "defs: \n~a\n" (map print-sham-def defs))
  (printf "ref: ~a\n"(print-sham-type ref)))
