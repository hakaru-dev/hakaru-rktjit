#lang racket

(require sham/ast)
(require "template-format.rkt")
(provide (all-defined-out))
(define type-nat-def (sham:def:type 'nat (sham:type:ref 'i64)))
(define type-real-def (sham:def:type 'real (sham:type:ref 'f64)))
(define type-prob-def (sham:def:type 'prob (sham:type:ref 'f64)))
(define type-bool-def (sham:def:type 'bool (sham:type:ref 'i1)))

(define type-nat-ref (sham:type:ref 'nat))
(define type-real-ref (sham:type:ref 'real))
(define type-prob-ref (sham:type:ref 'prob))
(define type-bool-ref (sham:type:ref 'bool))

(define (real-value v)
  (sham:exp:fl-value v type-real-ref))
(define (nat-value v)
  (sham:exp:ui-value v type-nat-ref))
(define (nat32-value v)
  (sham:exp:ui-value v (sham:type:ref 'i32)))
(define (prob-value v)
  (sham:exp:app (sham:rator:symbol'real2prob)
                (list (real-value (exact->inexact v)))))

(define (get-struct-field sym . indexs)
  (sham:exp:gep (sham$var sym) (cons (nat32-value 0) (map nat32-value indexs))))
(define (get-size-ptr vsym)
  (get-struct-field vsym 0))
(define (get-data-ptr vsym)
  (get-struct-field vsym 1))

(define (get-type-string t)
  (match t
    [`(pointer ,tp)
     (format pointer-format (get-type-string tp))]
    [`(array ,tar)
     (format array-format (get-type-string (if-need-pointer tar)))]
    [`(measure ,t) (get-type-string t)]
    [`(pair ,t1 ,t2) (format pair-format
                             (get-type-string (if-need-pointer t1))
                             (get-type-string (if-need-pointer t2)))]
    [nrp? (symbol->string t)]
    [else (error "unknown type format" t)]))
(define get-type-sym (compose string->symbol get-type-string))

(define nrp? (curryr member '(nat real prob int bool)))
(define (if-need-pointer t)
  (match t
    [`(measure ,mt) (if-need-pointer mt)]
    [tp #:when (nrp? t) tp]
    [else `(pointer ,t)]))
(define (expand-type t)
  (define et expand-type)
  (match t
    [`(pointer ,t) `(pointer ,(et t))]
    [`(array ,tar) `(pointer (array ,(et tar)))]
    [`(pair ,t1 ,t2) `(pointer (pair ,(et t1) ,(et t2)))]
    [nrp? t]))
(define sham-type-def-hash (make-hash))
(define type-hash (make-hash))
;; gets an type in hakrit format and
;; returns list of defines
;; first is for tast rest dependencies
(define (get-sham-type-define tast)
  (define (create-new!)
    (match tast
      [`(array ,t)
       (define st (get-sham-type-define `(pointer ,(if-need-pointer t))))
       (define ct (sham:def:type (get-type-sym tast)
                   (sham:type:struct array-args (list type-nat-ref (get-sham-type-ref (car st))))))
       (cons ct (cons type-nat-def st))]
      [`(pair ,t1 ,t2)
       (define st1 (get-sham-type-define (if-need-pointer t1)))
       (define st2 (get-sham-type-define (if-need-pointer t2)))
       (define ct (sham:def:type (get-type-sym tast)
                   (sham:type:struct (list pair-car-sym pair-cdr-sym)
                                     (list (get-sham-type-ref (car st1))
                                           (get-sham-type-ref (car st2))))))
       (cons ct (append st1 st2))]
      [`(measure ,t)
       (get-sham-type-define t)]
      [`(pointer ,t)
       (define st (get-sham-type-define t))
       (define ct (sham:def:type
                   (get-type-sym tast)
                   (sham:type:pointer (get-sham-type-ref (car st)))))
       (cons ct st)]
      ['nat (cons type-nat-def '())]
      ['int (cons type-nat-def '())]
      ['prob (cons type-prob-def '())]
      ['bool  (cons type-bool-def '())]
      ['real (cons type-real-def '())]))
  (define defs (hash-ref! sham-type-def-hash tast create-new!))
  (map (λ (d) (hash-set! type-hash (sham:def-id d) (sham:def:type-type d))) defs)
  defs)

(define sham-type-ref-hash (make-hash))
(define (get-sham-type-ref sham-type-def)
  (hash-ref! sham-type-ref-hash (sham:def-id sham-type-def)
             (λ () (sham:type:ref (sham:def-id sham-type-def)))))
(define (get-sham-type-ref-ast type-ast)
  (get-sham-type-ref (get-sham-type-define type-ast)))

(define (type-remove-pointer t)
  (match t
    [(sham:type:pointer to) to]
    [(sham:type:ref id)
     (type-remove-pointer (hash-ref type-hash id))]))

(define (defs-def-t-tref tast)
  (define defs (get-sham-type-define tast))
  (define def (car defs))
  (define t (sham:def:type-type def))
  (define tref (get-sham-type-ref def))
  (values defs def t tref))
