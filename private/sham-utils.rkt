#lang racket
(require sham/jit
         sham/ast)

(provide (all-defined-out))

(define pointer-format "~a*")
(define array-format "array<~a>")
(define array-args '(size data))

(define pair-format "pair<~a,~a>")
(define pair-car-sym 'a)
(define pair-cdr-sym 'b)
(define make-pair-fun-format "make-pair<~a>")
(define pair-car-fun-format "pair-car<~a>")
(define pair-cdr-fun-format "pair-cdr<~a>")

(define make-array-fun-format "make-array<~a>")
(define get-array-data-fun-format "get-array-data<~a>")
(define get-array-size-fun-format "get-array-size<~a>")
(define new-size-array-fun-format "new-size-array<~a>")
(define get-index-fun-format "get-index-array<~a>")
(define set-index-fun-format "set-index-array<~a>")
(define empty-array-fun-format "empty-array<~a>")

(define add-fun-format "add-~a-~a");;add-<num-args>-<type>
(define mul-fun-format "mul-~a-~a");;mul-<num-args>-<type>
(define recip-fun-format "recip-~a")
(define categorical-fun-format "categorical-~a");;categorical-<num-args>

(define type-nat-def (sham:def:type 'nat (sham:type:ref 'i32)))
(define type-real-def (sham:def:type 'real (sham:type:ref 'f64)))
(define type-prob-def (sham:def:type 'prob (sham:type:ref 'f64)))

(define type-nat-ref (sham:type:ref 'nat))
(define type-real-ref (sham:type:ref 'real))
(define type-prob-ref (sham:type:ref 'prob))

(define (real-value v)
  (sham:exp:fl-value v type-real-ref))
(define (nat-value v)
  (sham:exp:ui-value v type-nat-ref))

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

(define nrp? (curryr member '(nat real prob)))
(define (if-need-pointer t) (if (nrp? t) t `(pointer ,t)))

(define sham-type-def-hash (make-hash))
;; gets a type in hakrit format and
;; returns list of defines
;; first is for tast rest dependencies
(define (get-sham-type-define tast)
  (printf "get-sham-type: ~a\n" tast)
  (define (create-new!)
    (printf "creating new!: ~a\n" tast)
    (match tast
      [`(array ,t)
       (define st (get-sham-type-define (if-need-pointer t)))
       (define ct (sham:def:type
                   (get-type-sym tast)
                   (sham:type:struct array-args
                                     (list type-nat-ref
                                           (get-sham-type-ref (car st))))))
       (cons ct (cons type-nat-def st))]
      [`(pair ,t1 ,t2)
       (define st1 (get-sham-type-define (if-need-pointer t1)))
       (define st2 (get-sham-type-define (if-need-pointer t2)))
       (define ct (sham:def:type
                   (get-type-sym tast)
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
      ['prob (cons type-prob-def '())]
      ['real (cons type-real-def '())]))
  (hash-ref! sham-type-def-hash tast create-new!))

(define sham-type-ref-hash (make-hash))
(define (get-sham-type-ref sham-type-def)
  (hash-ref! sham-type-ref-hash (sham:def-id sham-type-def)
             (λ () (sham:type:ref (sham:def-id sham-type-def)))))
(define (get-sham-type-ref-ast type-ast)
  (get-sham-type-ref (get-sham-type-define type-ast)))

;;ret-type: symbol
;;rator: symbol
;;rand-types: (list symbol)
;;returns (list format-str args ...)
(define (get-rator-template ret-type rator rand-types)
  (match rator
    ['+ (list add-fun-format (length rand-types) (get-type-sym (car rand-types)))]
    ['* (list mul-fun-format (length rand-types) (get-type-sym (car rand-types)))]
    ['recip (list recip-fun-format (get-type-sym (car rand-types)))]
    ['empty (list make-array-fun-format (get-type-sym ret-type))]
    ['size (list get-array-size-fun-format (get-type-sym (car rand-types)))]
    ['index (list get-index-fun-format (get-type-sym (car rand-types)))]
    ['cons (list make-pair-fun-format (get-type-sym ret-type))]
    ['car (list pair-car-fun-format (get-type-sym (car rand-types)))]
    ['cdr (list pair-cdr-fun-format (get-type-sym (car rand-types)))]
    ['categorical (list categorical-fun-format (length rand-types))]))

