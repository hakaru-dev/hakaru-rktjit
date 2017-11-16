#lang racket

(require sham/ast
         (submod sham/ast utils))
(require "template-format.rkt"
         "utils.rkt")

(provide (all-defined-out))

(define type-nat-def (sham$def:type 'nat (sham:type:ref 'i64)))
(define type-int-def (sham$def:type 'int (sham:type:ref 'i64)))
(define type-real-def (sham$def:type 'real (sham:type:ref 'f64)))
(define type-prob-def (sham$def:type 'prob (sham:type:ref 'f64)))
(define type-bool-def (sham$def:type 'bool (sham:type:ref 'i1)))
(define type-void-def (sham$def:type 'void  (sham:type:ref 'void)))

(define type-nat-ref (sham:type:ref 'nat))
(define type-int-ref (sham:type:ref 'int))
(define type-real-ref (sham:type:ref 'real))
(define type-prob-ref (sham:type:ref 'prob))
(define type-bool-ref (sham:type:ref 'bool))
(define type-void-ref (sham:type:ref 'void))

(define t8* (sham:type:pointer (sham:type:ref 'i8)))
(define (treal? t)
  (equal? t 'real))
(define (tprob? t)
  (equal? t 'prob))
(define (tnat? t)
  (equal? t 'nat))
(define (tbool? t)
  (equal? t 'bool))
(define (tint? t)
  (equal? t 'int))

(define (real-value v)
  (sham:expr:fl-value (exact->inexact v) type-real-ref))
(define (nat-value v)
  (sham:expr:ui-value v type-nat-ref))
(define (nat32-value v)
  (sham:expr:ui-value v (sham:type:ref 'i32)))
(define (prob-value v)
  (sham:expr:fl-value (exact->inexact v) type-prob-ref))

(define (get-struct-field sym . indexs)
  (sham:expr:gep (sham$var sym) (cons (nat32-value 0) (map nat32-value indexs))))
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
    [`(array ,tar (size . ,s))
     (format sized-array-format s (get-type-string (if-need-pointer tar)))]
    [`(measure ,t) (get-type-string t)]
    [`(pair ,t1 ,t2) (format pair-format
                             (get-type-string (if-need-pointer t1))
                             (get-type-string (if-need-pointer t2)))]
    [`(struct-type (,ts ...)) (format "struct<~a>" (string-join (map get-type-string ts) "."))]
    [`(,t ,info) #:when (nrp? t) (symbol->string t)]
    [nrp? (symbol->string t)]
    [else (error "unknown type format" t)]))
(define get-type-sym (compose string->symbol get-type-string))

(define nrp? (curryr member '(nat real prob int bool unit i1)))
(define (remove-measure t)
  (match t
    [`(measure ,t) t]
    [else t]))
(define (if-need-pointer t)
  (match t
    [`(measure ,mt) (if-need-pointer mt)]
    [`(,t ,i) #:when (nrp? t) t]
    [tp #:when (nrp? t) tp]
    [else `(pointer ,t)]))

(define (clean-type-info ti)
  (match ti
    [`(,t ,_) #:when (nrp? t) t]
    [`(array ,t ,_) (clean-type-info t)]
    [`(pair ,t1 ,t2) `(pair ,(clean-type-info t1)
                            ,(clean-type-info t2))]
    [`(measure ,t) (clean-type-info t)]
    [else ti]))

(define (pointer-type? t)
  (and (pair? (if-need-pointer t))
       (equal? (car (if-need-pointer t)) 'pointer)))

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
       (define ct (sham$def:type (get-type-sym tast)
                                 (sham:type:struct array-args (list type-nat-ref (get-sham-type-ref (car st))))))
       (cons ct (cons type-nat-def st))]
      [`(array ,t (size . ,s))
       (define st (get-sham-type-define (if-need-pointer t)))
       (define def (sham$def:type (get-type-sym tast) (sham:type:array (get-sham-type-ref (car st)) s)))
       (cons def st)]

      [`(struct-type ,arg-types)
       (define arg-defs (map (compose get-sham-type-define if-need-pointer) arg-types))
       (define st (sham$def:type
                   (get-type-sym tast)
                   (sham:type:struct (build-list (length arg-types) get-vi)
                                     (map (compose get-sham-type-ref car) arg-defs))))
       (cons st (apply append arg-defs))]
      [`(pair ,t1 ,t2)
       (define st1 (get-sham-type-define (if-need-pointer t1)))
       (define st2 (get-sham-type-define (if-need-pointer t2)))
       (define ct (sham$def:type (get-type-sym tast)
                                 (sham:type:struct (list pair-car-sym pair-cdr-sym)
                                                   (list (get-sham-type-ref (car st1))
                                                         (get-sham-type-ref (car st2))))))
       (cons ct (append st1 st2))]
      [`(measure ,t)
       (get-sham-type-define t)]
      [`(pointer ,t)
       (define st (get-sham-type-define t))
       (define ct (sham$def:type
                   (get-type-sym tast)
                   (sham:type:pointer (get-sham-type-ref (car st)))))
       (cons ct st)]
      ['nat (list type-nat-def)]
      ['int (list type-nat-def)]
      ['prob (list type-prob-def)]
      ['bool  (list type-bool-def)]
      ['real (list type-real-def)]
      ['unit (list type-nat-def)]
      ['void (list type-void-def)]
      [`(nat ,info) (list type-nat-def)]
      [`(prob ,info) (list type-prob-def)]))
  (define defs (hash-ref! sham-type-def-hash tast create-new!))
  (map (λ (d) (hash-set! type-hash (sham:def-id d) (sham:def:type-type d))) defs)
  defs)

(define sham-type-ref-hash (make-hash))
(define (get-sham-type-ref sham-type-def)
  (hash-ref! sham-type-ref-hash (sham:def-id sham-type-def)
             (λ () (sham:type:ref (sham:def-id sham-type-def)))))
(define (get-sham-type-ref-ast type-ast)
  (get-sham-type-ref (car (get-sham-type-define type-ast))))

(define (type-remove-pointer t)
  (match t
    [(sham:type:pointer _ to) to]
    [(sham:type:ref _ id)
     (type-remove-pointer (hash-ref type-hash id))]))

(define (defs-def-t-tref tast)
  (define defs (get-sham-type-define tast))
  (define def (car defs))
  (define t (sham:def:type-type def))
  (define tref (get-sham-type-ref def))
  (values defs def t tref))
