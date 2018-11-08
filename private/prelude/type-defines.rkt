#lang racket

(require "../../../sham/private/ast-utils.rkt"
         ;; (submod sham/ast utils)
         )
(require "template-format.rkt"
         "utils.rkt")

(provide (all-defined-out))

(define tnat i64)
(define tint i64)
(define treal f64)
(define tprob f64)
(define tbool i1)
(define tarray i8*)

(define (treal? t) (equal? t treal))
(define (tprob? t) (equal? t tprob))
(define (tnat?  t) (equal? t tnat))
(define (tbool? t) (equal? t tbool))
(define (tint?  t) (equal? t tint))

(define (nat-value v) (ui32 v))
(define (int-value v) (si32 v))
(define (bool-value v) (ui1 (if (equal? v 0) 0 1)))
(define (real-value v) (fl64 (exact->inexact v)))
(define (prob-value v) (app (rs 'real2prob) (real-value v)))

(define (create-tarray type)
  (tstruct (list 'size 'ptr) (list tnat (tptr type))))
(define (create-fixed-tarray type size)
  (tarr type size))
(define (create-tpair t1 t2)
  (tstruct '(a d) (list t1 t2)))

(define (get-sham-type hakrit-type)
  (match hakrit-type
    [`(array ,t) i8*]
    [`(array ,t (size . ,s)) i8*]
    [`(pair ,t1 ,t2) i8*]
    [`(measure ,t) (get-sham-type t)]
    [`(pointer ,t) i8*]
    [`nat tnat]
    [`int tint]
    [`prob tprob]
    [`real treal]
    [`bool tbool]
    [`unit tnat]
    [`void tvoid]
    [`(nat ,info) tnat]
    [`(prob ,info) tprob]))

(define nrp? (curryr member '(nat real prob int bool unit i1)))
(define (remove-measure t)
  (match t [`(measure ,t) t] [else t]))

(define (if-need-pointer t)
  (match t
    [`(measure ,mt) (if-need-pointer mt)]
    [`(,t ,i) #:when (nrp? t) t]
    [tp #:when (nrp? t) tp]
    [else `(pointer ,t)]))

(define (clean-tinfo ti)
  (match ti
    [`(,t ,_) #:when (nrp? t) t]
    [`(array ,t ,_) (clean-tinfo t)]
    [`(pair ,t1 ,t2) `(pair ,(clean-tinfo t1)
                            ,(clean-tinfo t2))]
    [`(measure ,t) (clean-tinfo t)]
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

(define (get-tstring t)
  (match t
    [`(pointer ,tp)
     (format pointer-format (get-tstring tp))]
    [`(array ,tar)
     (format array-format (get-tstring (if-need-pointer tar)))]
    [`(array ,tar (size . ,s))
     (format sized-array-format s (get-tstring tar))]
    [`(measure ,t) (get-tstring t)]
    [`(pair ,t1 ,t2) (format pair-format
                             (get-tstring (if-need-pointer t1))
                             (get-tstring (if-need-pointer t2)))]
    [`(struct-type (,ts ...)) (format "struct<~a>" (string-join (map get-tstring ts) "."))]
    [`(,t ,info) #:when (nrp? t) (symbol->string t)]
    [nrp? (symbol->string t)]
    [else (error "unknown type format" t)]))
(define get-tsym (compose string->symbol get-tstring))

(define (get-function-id frmt type)
  (define ts (get-tstring type))
  (string->symbol (format frmt ts)))
#|
(define (real-value v)
  (sham:expr:fl-value (exact->inexact v) treal-ref))
(define (nat-value v)
  (sham:expr:ui-value v tnat-ref))
(define (nat32-value v)
  (sham:expr:ui-value v (sham:type:ref 'i32)))
(define (prob-value v)
  (sham:expr:app (sham:rator:symbol 'real2prob) (list  (sham:expr:fl-value (exact->inexact v) tprob-ref))))

(define (get-struct-field sym . indexs)
  (sham:expr:gep (sham$var sym) (cons (nat32-value 0) (map nat32-value indexs))))
(define (get-size-ptr vsym)
  (get-struct-field vsym 0))
(define (get-data-ptr vsym)
  (get-struct-field vsym 1))





(define sham-tdef-hash (make-hash))
(define thash (make-hash))
;; gets an type in hakrit format and
;; returns list of defines
;; first is for tast rest dependencies
(define (get-sham-tdefine tast)

  (define (create-new!)
    (match tast
      [`(array ,t)
       (define st (get-sham-tdefine `(pointer ,(if-need-pointer t))))
       (define ct (sham$def:type (get-tsym tast)
                                 (sham:type:struct array-args (list tnat-ref (get-sham-tref (car st))))))
       (cons ct (cons tnat-def st))]
      [`(array ,t (size . ,s))
       (define st (get-sham-tdefine t;; (if-need-pointer t)
                                        ))
       (define def (sham$def:type (get-tsym tast) (sham:type:array (get-sham-tref (car st)) s)))
       (cons def st)]

      [`(struct-type ,arg-types)
       (define arg-defs (map (compose get-sham-tdefine if-need-pointer) arg-types))
       (define st (sham$def:type
                   (get-tsym tast)
                   (sham:type:struct (build-list (length arg-types) get-vi)
                                     (map (compose get-sham-tref car) arg-defs))))
       (cons st (apply append arg-defs))]
      [`(pair ,t1 ,t2)
       (define st1 (get-sham-tdefine (if-need-pointer t1)))
       (define st2 (get-sham-tdefine (if-need-pointer t2)))
       (define ct (sham$def:type (get-tsym tast)
                                 (sham:type:struct (list pair-car-sym pair-cdr-sym)
                                                   (list (get-sham-tref (car st1))
                                                         (get-sham-tref (car st2))))))
       (cons ct (append st1 st2))]
      [`(measure ,t)
       (get-sham-tdefine t)]
      [`(pointer ,t)
       (define st (get-sham-tdefine t))
       (define ct (sham$def:type
                   (get-tsym tast)
                   (sham:type:pointer (get-sham-tref (car st)))))
       (cons ct st)]
      ['nat (list tnat-def)]
      ['int (list tnat-def)]
      ['prob (list tprob-def)]
      ['bool  (list tbool-def)]
      ['real (list treal-def)]
      ['unit (list tnat-def)]
      ['void (list tvoid-def)]
      [`(nat ,info) (list tnat-def)]
      [`(prob ,info) (list tprob-def)]))
  (define defs (hash-ref! sham-tdef-hash tast create-new!))
  (map (λ (d) (hash-set! thash (sham:def-id d) (sham:def:ttype d))) defs)
  defs)

(define sham-tref-hash (make-hash))
(define (get-sham-tref sham-tdef)
  (hash-ref! sham-tref-hash (sham:def-id sham-tdef)
             (λ () (sham:type:ref (sham:def-id sham-tdef)))))
(define (get-sham-tref-ast tast)
  (get-sham-tref (car (get-sham-tdefine tast))))

(define (tremove-pointer t)
  (match t
    [(sham:type:pointer _ to) to]
    [(sham:type:ref _ id)
     (tremove-pointer (hash-ref thash id))]))

(define (defs-def-t-tref tast)
  (define defs (get-sham-tdefine tast))
  (define def (car defs))
  (define t (sham:def:ttype def))
  (define tref (get-sham-tref def))
  (values defs def t tref))
|#
