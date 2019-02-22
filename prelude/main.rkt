#lang racket

(require "type-defines.rkt"
         "basic-defines.rkt"
         "array-defines.rkt"
         "pair-defines.rkt"
         "struct-defines.rkt"
         "probability-defines.rkt")

(provide basic-mod-info
         prelude-module
         ;; prog-fun-info => basic-fun-info
         get-sham-rator
         get-sham-value
         get-sham-type ;; type-defines.rkt
         ;; (all-from-out "type-defines.rkt"
         ;;               "basic-defines.rkt"
         ;;               "array-defines.rkt"
         ;;               "pair-defines.rkt"
         ;;               "probability-defines.rkt")
         )


(define (get-sham-rator rator tresult trands)
  ;; (printf "get-sham-rator: ~a, ~a, ~a\n" rator tresult trands)
  (match rator
    [(? array-rator?) (get-array-rator rator tresult trands)]
    [(? pair-rator?) (get-pair-rator rator)]
    [(? basic-rator?) (get-basic-rator rator tresult trands)]
    [(? probability-rator?) (get-probability-rator rator tresult trands)]
    [else (error "unknown rator in get-sham-rator." rator)]))

(define (get-sham-value v t)
  ;; (printf "get-sham-value: ~a ~a\n" v t)
  (match t
    ['prob (prob-value v)]
    ['nat (nat-value v)]
    ['int (int-value v)]
    ['real (real-value v)]
    ['bool (bool-value v)]
    ['(measure bool) (bool-value v)]))


;;returns (values type-ref (list type-defs ...))
;; (define (get-sham-type type-ast)
;;   (sham-type type-ast))

#;(define (get-sham-defs type-ast)
  (define tast (if-need-pointer type-ast))
  (match type-ast
    [`(array ,arr-t)
     (simple-array-defines tast)]
    ;; [`(array ,arr-t (size . ,t))
    ;;  (define-values (_ odefs)
    ;;    (get-defs&ref-type arr-t))
    ;;  (append (const-array-defs type-ast) odefs)]
    ;; [`(struct-type (,ts ...))
    ;;  (define tsdefs (map (λ (t) (define-values (_ odefs) (get-defs&ref-type t))  odefs) ts))
    ;;  (append (get-struct-defs type-ast) tsdefs)]
    ;; [`(pair ,ta ,tb)
    ;;  (define-values (taref tadefs) (get-defs&ref-type ta))
    ;;  (define-values (tbref tbdefs) (get-defs&ref-type tb))
    ;;  (append (pair-defs type-ast)
    ;;          tadefs
    ;;          tbdefs)]
    [`(measure ,t) (define-values (_ defs) (get-defs&ref-type t)) defs]
    ['nat (list type-nat-def)]
    ['int (list type-nat-def)]
    ['prob (list type-prob-def)]
    ['bool  (list type-bool-def)]
    ['real (list type-real-def)]
    ['unit (list type-nat-def)]
    ['void (list type-void-def)]
    [`(,t ,_) #:when (nrp? t) (define-values (ref def) (get-defs&ref-type t)) def]
    [else (error "unknown type" type-ast)]))


;; (define (get-rator sym tresult trands)
;;   ((match sym
;;      [(? basic-rator?) get-basic-rator]
;;      [(? pair-rator?) get-pair-rator]
;;      [(? array-rator?) get-array-rator]
;;      [(? probability-rator?) get-probability-rator]
;;      [(? struct-rator?) get-struct-rator]
;;      [else (error "why is this rator not done yet?" sym tresult trands)])
;;    sym (remove-measure tresult) (map remove-measure trands)))

;; (define (get-value v type)
;;   (match (clean-type-info type)
;;     ['nat (nat-value (truncate (inexact->exact v)))]
;;     ['int (int-value (truncate (inexact->exact v)))]
;;     ['bool (bool-value (truncate (inexact->exact v)))]
;;     ['unit (nat-value (truncate (inexact->exact 0)))]
;;     ['prob (prob-value (exact->inexact v))]
;;     ['real (real-value (exact->inexact v))]
;;     [`(measure ,t) (get-value v t)]))

;; (module+ test
;;   (define-values
;;     (ref defs)
;;     (get-defs&ref-type
;;      '(array (pair (array prob) (pair real prob)))))
;;   (printf "defs: \n~a\n" (map print-sham-def defs))
;;   (printf "ref: ~a\n"(print-sham-type ref)))
