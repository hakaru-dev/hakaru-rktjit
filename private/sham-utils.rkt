#lang racket
(require sham/jit
         sham/ast)

(provide (all-defined-out))

(define pointer-format "~a*")
(define array-format "array<~a>")
(define measure-format "measure<~a>")
(define pair-format "pair<~a,~a>")

(define pair-car-sym 'a)
(define pair-cdr-sym 'b)

(define type-nat-def (sham:type:ref 'i32))
(define type-real-def (sham:type:ref 'f64))
(define type-prob-def (sham:type:ref 'f64))

(define type-nat-ref (sham:type:ref 'i32))
(define type-real-ref (sham:type:ref 'f64))
(define type-prob-ref (sham:type:ref 'f64))

(define (real-value v)
  (sham:exp:fl-value v type-real-ref))
(define (nat-value v)
  (sham:exp:ui-value v type-nat-ref))

(define sham-type-def-hash (make-hash))


;; gets a type in hakrit format and
;; returns two values (type type-dependencies)
(define (get-sham-type-define tast)
  (define (if-need-pointer t) (not (member t '(nat real prob))))
  (printf "to create-type: ~a\n" tast)
  (match tast
    [`(array ,t)
     (define-values (st std) (get-sham-type-define (if-need-pointer t)))
     (define ct (sham:def:type
                 (string->symbol (format array-format (sham:def:type-id st)))
                 (sham:type:struct '(size data)
                                    (list type-nat-ref
                                          (get-sham-type-ref st)))))
     (values ct (append (list st type-nat-ref) std))]
    [`(pair ,t1 ,t2)
     (define-values (st1 std1) (get-sham-type-define (if-need-pointer t1)))
     (define-values (st2 std2) (get-sham-type-define (if-need-pointer t2)))
     (define ct (sham:def:type
                 (string->symbol (format pair-formt
                                         (sham:def:type-id st1)
                                         (sham:def:type-id st2)))
                 (sham:type:struct (list pair-car-sym pair-cdr-sym)
                                   (list (get-sham-type-ref st1)
                                         (get-sham-type-ref st2))))
       (values ct (append (list st1 st2) std1 std2)))]
    [`(measure ,t)
     (get-sham-type-define t)]
    [`(pointer ,t)
     (define-values (st std) (get-sham-type-define t))
     (define ct (sham:def:type
                 (string->symbol (format pointer-format
                                         (sham:def:type-id st)))
                 (sham:type:pointer (get-sham-type-ref st))))
     (values ct (cons st std))]
    ['nat (values type-nat-def '())]
    ['prob (values type-prob-def '())]
    ['real (values type-real-def '())]))

(define sham-type-ref-hash (make-hash))
(define (get-sham-type-ref sham-type-def)
  0) 
