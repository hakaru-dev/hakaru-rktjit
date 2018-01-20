#lang racket

(require sham
         (submod sham/ast utils))
(require ffi/unsafe)
(require "template-format.rkt"
         "type-defines.rkt"
         "basic-defines.rkt"
         "utils.rkt")

(provide array-defs
         const-array-defs
         array-rator?
         get-array-rator
         build-array-literal)

(define debug-arrays (make-parameter #f))
(define (array-rator? sym)
   (member sym '(empty index size set-index! array-literal
                       free const-size-array-literal)))


(define (get-array-rator sym tresult trands)
;  (printf "getting array rator: ~a ~a ~a\n" sym tresult trands)
  (cond
    [(equal? sym 'array-literal)
     (values (sham:rator:symbol (get-fun-symbol array-literal-fun-format (length trands) (get-type-string tresult)))
             (build-array-literal tresult (length trands)))]
    [(equal? sym 'const-size-array-literal)
     (values (sham:rator:symbol (get-fun-symbol size-array-literal-fun-format (get-type-string tresult)))
             (void))]
    [else (values
           (sham:rator:symbol
            (string->symbol
             (call-with-values
              (λ ()
                (match sym
                  ['index (values get-index-fun-format (get-type-string (car trands)))]
                  ['set-index! (values set-index-fun-format (get-type-string (car trands)))]
                  ['size (values get-array-size-fun-format (get-type-string (car trands)))]
                  ['empty (values new-size-array-fun-format (get-type-string tresult))]
                  ['free (values free-size-array-fun-format (get-type-string (car trands)))]
                  [else
                   (error "why is this array rator not done yet?" sym tresult trands)
                   (values "array?")]))
              format)))
           (void))]))

(define (get-array-data-type type)
  (match type
    [(sham:type:struct _ _ (list _ t)) t]))

;;tast be expanded
(define (array-defs tast)
  (define-values
    (atdefs atdef at atref) (defs-def-t-tref tast))
  (define-values
    (aptdefs aptdef apt aptref) (defs-def-t-tref `(pointer ,tast)))
  (define adtref (get-array-data-type at))
  (define adpt (sham:type:pointer adtref))
  (define adnptref (type-remove-pointer adtref))
  (define nat type-nat-ref)
  (define nat* (sham:type:pointer nat))
  (define arr-id (sham:def-id atdef))
  (define (get-fun-name frmt)
    (string->symbol (format frmt arr-id)))

  (define (make-array)
    (sham:def:function ;make-array
     (prelude-function-info)
     (get-fun-name make-array-fun-format)
     array-args (list nat adtref) aptref
     (sham:stmt:let
      '(ap* ap-size* ap-data*)
      (list aptref nat* adpt)
      (list (sham$app 'malloc (sham:expr:type atref))
            (get-size-ptr 'ap*)
            (get-data-ptr 'ap*))
      (sham$block
       (sham:stmt:expr (sham$app store! size ap-size*))
       (sham:stmt:expr (sham$app store! data ap-data*))
       (sham:stmt:return (sham$var 'ap*))))))

  (define (new-size-array)
    (sham:def:function ;new-size-array
     (prelude-function-info)
     (get-fun-name new-size-array-fun-format)
     '(size) (list nat) aptref
     (sham:stmt:let
      '(apt)
      (list aptref)
      (list (sham:expr:app
             (sham:rator:symbol (get-fun-name make-array-fun-format))
             (list (sham$var 'size)
                   (sham$app arr-malloc
                             (sham:expr:type adnptref) (sham$var 'size)))))
      (sham:stmt:block
       (list
        (sham:stmt:void)
        (sham:stmt:expr
         (sham:expr:app
          (sham:rator:intrinsic 'llvm.memset.p0i8.i64 (sham:type:ref 'void))
          (list (sham$app ptrcast (sham$app 'load (get-data-ptr 'apt)) (sham:expr:type (sham:type:pointer
                                                                                       (sham:type:ref 'i8))))
                (sham$app intcast (nat-value 0) (sham:expr:type (sham:type:ref 'i8)))
                (sham$app mul-nuw (sham$app intcast (sham:expr:sizeof nat) (sham$etype i64)) (sham$var 'size))
                (sham$app intcast (nat-value 0) (sham$etype i32))
                (sham$app intcast (nat-value 0) (sham$etype i1)))))

        (sham:stmt:return (sham$var 'apt)))))))

  (define (free-size-array)
    (sham:def:function
     (prelude-function-info)
     (get-fun-name free-size-array-fun-format)
     '(ap*) (list aptref) (sham:type:ref 'void)
     (sham$block
      (sham:stmt:expr (sham:expr:app (sham:rator:symbol 'free) (list (sham$app 'load (get-data-ptr 'ap*)))))
      (sham:stmt:expr (sham:expr:app (sham:rator:symbol 'free) (list (sham$var 'ap*))))
      (sham:stmt:return (sham:expr:void)))))

  (define (get-array-data)
    (sham:def:function ;get-array-data
     (prelude-function-info)
     (get-fun-name get-array-data-fun-format)
     '(ap*) (list aptref) adtref
     (sham:stmt:let
      '(adt*)
      (list adpt)
      (list (get-data-ptr 'ap*))
      (sham:stmt:return (sham$app 'load (sham$var 'adt*))))))

  (define (get-array-size)
    (sham:def:function ;get-array-size
     (prelude-function-info)
     (get-fun-name get-array-size-fun-format)
     '(array-ptr) (list aptref) nat
     (sham:stmt:return (sham$app 'load (get-size-ptr 'array-ptr)))))

  (define (get-index-error)
    (sham:def:function
     (prelude-function-info)
     (get-fun-name get-index-error-fun-format) (list 'a 'b 'c) (list nat nat aptref)
     (sham:type:ref 'void)
     (sham:stmt:block
      (list
       (sham:stmt:expr
        (sham:expr:app
         (sham:rator:racket
          (get-fun-name rkt-get-index-error-fun-format)
          (λ (a b c) (printf "wrong index while calling get-index~a: ~a ~a, ~a\n"
                             tast a b (map exp (cblock->list c _double 20))))
          (sham:type:function (list nat nat (sham:type:pointer (sham:type:ref 'i8)))
                              (sham:type:ref 'void)))
         (list (sham$var a) (sham$var b) (sham$var c))))
       (sham:stmt:return (sham:expr:void))))))

  (define (get-index)
    (sham:def:function ;get-index
     (prelude-function-info)
     (get-fun-name get-index-fun-format)
     '(array-ptr index)
     (list aptref nat ) adnptref
     (if (debug-arrays)
         (sham:stmt:if
          (sham$app icmp-uge index
                    (sham:expr:app
                     (sham:rator:symbol (get-fun-name get-array-size-fun-format))
                     (list (sham$var 'array-ptr))))
          (sham$block (sham:stmt:expr
                       (sham:expr:app (sham:rator:symbol (get-fun-name get-index-error-fun-format))
                                      (list (sham$var 'index)
                                            (sham:expr:app
                                             (sham:rator:symbol (get-fun-name get-array-size-fun-format))
                                             (list (sham$var 'array-ptr)))
                                            (sham$app 'load (get-data-ptr 'array-ptr)))))
                      (sham:stmt:return (sham$app 'load
                                                  (sham:expr:gep (sham$app 'load (get-data-ptr 'array-ptr))
                                                                 (list (sham:expr:ui-value 0 nat))))))
          (sham:stmt:return
           (sham$app 'load
                     (sham:expr:gep (sham$app 'load (get-data-ptr 'array-ptr))
                                    (list (sham$var 'index))))))
         (sham:stmt:return
           (sham$app 'load
                     (sham:expr:gep (sham$app 'load (get-data-ptr 'array-ptr))
                                    (list (sham$var 'index))))))))

  (define (set-index-error)
    (sham:def:function
     (prelude-function-info)
     (get-fun-name set-index-error-fun-format) (list 'a 'b) (list nat nat) (sham:type:ref 'void)
     (sham:stmt:block
      (list
       (sham:stmt:expr
        (sham:expr:app
         (sham:rator:racket
          (get-fun-name rkt-set-index-error-fun-format)
          (λ (a b) (printf "wrong index while calling set-index: ~a ~a\n" a b))
          (sham:type:function (list nat nat)
                              (sham:type:ref 'void)))
         (list (sham$var a) (sham$var b))))
       (sham:stmt:return (sham:expr:void))))))



  (define (set-index)
    (sham:def:function ;set-index
     (prelude-function-info)
     (get-fun-name set-index-fun-format)
     '(array-ptr index v)
     (list aptref nat adnptref) (sham:type:ref 'void)
     (if (debug-arrays)
         (sham:stmt:if
          (sham$app icmp-uge index
                    (sham:expr:app (sham:rator:symbol (get-fun-name get-array-size-fun-format))
                                   (list (sham$var 'array-ptr))))
          (sham$block (sham:stmt:expr
                       (sham:expr:app (sham:rator:symbol (get-fun-name set-index-error-fun-format))
                                      (list (sham$var 'index)
                                            (sham:expr:app (sham:rator:symbol (get-fun-name get-array-size-fun-format))
                                                           (list (sham$var 'array-ptr))))))
                      (sham:stmt:return (sham:expr:void)))
          (sham$block
           (sham:stmt:expr
            (sham$app store! (sham$var 'v)
                      (sham:expr:gep (sham$app 'load (get-data-ptr 'array-ptr))
                                     (list (sham$var 'index)))))
           (sham:stmt:return (sham:expr:void))))
         (sham$block
          (sham:stmt:expr
           (sham$app store! (sham$var 'v)
                     (sham:expr:gep (sham$app 'load (get-data-ptr 'array-ptr))
                                    (list (sham$var 'index)))))
          (sham:stmt:return (sham:expr:void))))))
  (define (empty-array)
    (sham:def:function ;empty-array
     (prelude-function-info)
     (get-fun-name empty-array-fun-format)
     '() '() aptref
     (sham:stmt:return
      (sham:expr:app (sham:rator:symbol (get-fun-name new-size-array-fun-format))
                     (list (nat-value 0))))))
  (append
   (reverse atdefs)
   (reverse aptdefs)
   (list
    (make-array)
    (new-size-array)
    (free-size-array)
    (empty-array)
    (get-array-size)
    (get-array-data)
    (get-index)
    (get-index-error)
    (set-index)
    (set-index-error))))

(define (const-array-defs tast)
  (define (get-array-data-type at)
    (match at
      [(sham:type:array _ t _) t]))
  (match-define `(array ,dtype (size . ,size)) tast)

  (define-values (atdefs atdef at atref) (defs-def-t-tref tast))
  (define-values (aptdefs aptdef apt aptref) (defs-def-t-tref `(pointer ,tast)))

  (define adtref (get-array-data-type at))

  (define adpt (sham:type:pointer adtref))
  (define nat type-nat-ref)
  (define nat* (sham:type:pointer nat))

  (define arr-id (sham:def-id atdef))
  (define (get-fun-name frmt)
    (string->symbol (format frmt arr-id)))

  (define (make-array)
    (sham:def:function
     (prelude-function-info)
     (get-fun-name new-size-array-fun-format)
     '() '() aptref
     (sham:stmt:return
      (sham$app bitcast
                (sham$app arr-malloc
                          (sham:expr:type atref) (sham:expr:ui-value size nat))
                (sham:expr:type aptref)))))
  (define (free-array)
    (sham:def:function
     (prelude-function-info)
     (get-fun-name free-size-array-fun-format)
     '(arr) (list aptref) (sham:type:ref 'void)
     (sham$block
      (sham:stmt:expr
       (sham:expr:app (sham:rator:symbol 'free) (list (sham$var 'arr))))
      (sham:stmt:return (sham:expr:void)))))

  (define (get-array-size)
    (sham:def:function
     (prelude-function-info)
     (get-fun-name get-array-size-fun-format)
     '(arr) (list aptref) nat
     (sham:stmt:return (sham:expr:ui-value size nat))))

  (define (get-index)
    (sham:def:function
     (prelude-function-info)
     (get-fun-name get-index-fun-format)
     '(arr ind) (list aptref nat) adtref
     (sham:stmt:return
      (sham$app 'load
                (sham:expr:gep (sham$var 'arr)
                               (list (nat32-value 0) (sham$var ind)))))))

  (define (set-index)
    (sham:def:function
     (prelude-function-info)
     (get-fun-name set-index-fun-format)
     '(arr ind v) (list aptref nat adtref) (sham:type:ref 'void)
     (sham$block
      (sham:stmt:expr (sham$app store! (sham$var v) (sham:expr:gep (sham$var 'arr) (list (nat32-value 0) (sham$var ind)))))
      (sham:stmt:return (sham:expr:void)))))
  (append
   (reverse atdefs)
   (reverse aptdefs)
   (list type-nat-def)
   (list
    (make-array)
    (free-array)
    (get-array-size)
    (get-index)
    (set-index))))

;; (define (build-array-literal)
;;     (sham:def:function
;;      (prelude-function-info)
;;      (get-fun-name size-array-literal-fun-format)
;;      (build-list size get-vi)
;;      (build-list size (const adtref)) aptref
;;      (sham:stmt:let
;;       '(arl) (list aptref)
;;       (list (sham:expr:app (sham:rator:symbol (get-fun-name new-size-array-fun-format)) '()))
;;       (sham:stmt:block
;;        (append (build-list size
;;                            (λ (i)
;;                              (sham:stmt:expr
;;                               (sham:expr:app (sham:rator:symbol (get-fun-name set-index-fun-format))
;;                                              (list (sham$var 'arl) (nat-value i) (sham$var (get-vi i)))))))
;;                (list (sham:stmt:return (sham$var 'arl))))))))

;;only build array-literals when called an nowhere else
(define (build-array-literal type len)
  (define tast type)
  (define-values
    (atdefs atdef at atref) (defs-def-t-tref tast))
  (define-values
    (aptdefs aptdef apt aptref) (defs-def-t-tref `(pointer ,tast)))
  (define adtref (get-array-data-type at))
  (define adpt (sham:type:pointer adtref))
  (define adnptref (type-remove-pointer adtref))
  (define nat type-nat-ref)
  (define nat* (sham:type:pointer nat))
  (define arr-id (sham:def-id atdef))

  (define type-sym (get-type-sym type))
  (define type-ref (sham:type:ref type-sym))
  (define array-sym (get-type-sym `(array ,type)))
  (define array-ref (sham:type:ref array-sym))
  (define arrayp-sym (get-type-sym `(pointer (array ,type))))
  (define arrayp-ref (sham:type:ref arrayp-sym))

  (sham:def:function
   (prelude-function-info)
   (string->symbol (format array-literal-fun-format len arr-id))
   (build-list len get-vi)
   (build-list len (const adnptref)) aptref
   (sham:stmt:let
    '(arl) (list aptref)
    (list (sham:expr:app
           (sham:rator:symbol (string->symbol (format new-size-array-fun-format arr-id)))
           (list (nat-value len))))
    (sham:stmt:block
     (append (build-list len
                         (λ (i)
                           (sham:stmt:expr
                            (sham:expr:app (sham:rator:symbol (get-fun-symbol set-index-fun-format arr-id))
                                           (list (sham$var 'arl) (nat-value i) (sham$var (get-vi i)))))))
             (list (sham:stmt:return (sham$var 'arl))))))))


(module+ test
  (require rackunit
           sham/jit
           ffi/unsafe
           "../../utils.rkt")

  (define defs
    (append  (append-map const-array-defs
                         `((array (array nat (size . 10)) (size . 10))
                           (array real (size . 10))))
            (append-map array-defs
                        `((array nat)
                          (array real)
                          (array prob)
                          (array (array nat))))
            (list (build-array-literal `(array real) 3))))



  (pretty-print (map print-sham-def defs))
  (define mod
    (sham:module
     (basic-mod-info)  defs))

  (define cmod (compile-module mod))
  ;; (jit-dump-module cmod)
  ;; (optimize-module cmod)
  ;  (jit-dump-module cmod)
  (printf "verify after optimize: ~a\n" (jit-verify-module cmod))
  (initialize-jit! cmod)
  ;(pretty-print cmod)
  (define (get-t t) (jit-get-racket-type t cmod))
  (define (get-f f) (jit-get-function f cmod))

  (define t-real (get-t 'real))
  (define t-nat (get-t 'nat))
  (define t-prob (get-t 'prob))
  (define e 0.00000000001)

  (define ta '(1.0 2.0 3.0 3.14 42.23))
  (define ti '(1 2 3 4 42))
  (define test-nat-array (list->cblock ti t-nat))
  (define test-real-array (list->cblock ta t-real))
  (define test-prob-array (list->cblock (map real->prob ta) t-prob))

  (define ans '(array nat))
  (define ars '(array real))
  (define aps '(array prob))
  (define aans '(array (array nat)))
  (define ((gf frmt) tsym)
    (get-f (get-fun-symbol frmt (get-type-string tsym))))

  (define make-f (gf make-array-fun-format))
  (define new-size-f (gf new-size-array-fun-format))
  (define free-f (gf free-size-array-fun-format))
  (define empty-f (gf empty-array-fun-format))
  (define size-f (gf get-array-size-fun-format))
  (define data-f (gf get-array-data-fun-format))
  (define index-f (gf get-index-fun-format))
  (define index!-f (gf set-index-fun-format))


  (define make-array-nat (make-f ans))
  (define new-sized-array-nat (new-size-f ans))
  (define empty-array-nat (empty-f ans))
  (define free-array-nat (free-f ans))
  (define get-data-array-nat (data-f ans))
  (define get-size-array-nat (size-f ans))
  (define get-index-array-nat (index-f ans))
  (define set-index-array-nat (index!-f ans))

  (define tiarr (make-array-nat (length ti) test-nat-array))
  (define eti (new-sized-array-nat 5))
  (check-eq? (get-size-array-nat tiarr) 5)
  (check-eq? (get-index-array-nat tiarr 4) 42)
  (set-index-array-nat tiarr 3 23)
  (check-eq? (get-index-array-nat tiarr 3) 23)

  (check-eq? (get-size-array-nat eti) 5)

  (set-index-array-nat eti 3 42)

  (check-eq? (get-index-array-nat eti 3) 42)

  (define make-array-real (make-f ars))
  (define new-sized-array-real (new-size-f ars))
  (define empty-array-real (empty-f ars))
  (define get-data-array-real (data-f ars))
  (define get-size-array-real (size-f ars))
  (define get-index-array-real (index-f ars))
  (define set-index-array-real (index!-f ars))

  (define trarr (make-array-real (length ti) test-real-array))
  (define etr (new-sized-array-real 5))
  (for ([i (in-range 5)])
    (check-eq? (get-index-array-real etr i) 0.0))
  (check-eq? (get-size-array-real trarr) 5)
  (check-= (get-index-array-real trarr 4) 42.23 e)
  (set-index-array-real trarr 3 23.42)
  (check-= (get-index-array-real trarr 3) 23.42 e)

  (check-eq? (get-size-array-real etr) 5)
  (set-index-array-real etr 3 42.23)
  (check-= (get-index-array-real etr 3) 42.23 e)

  (define make-array-prob (make-f aps))
  (define new-sized-array-prob (new-size-f aps))
  (define empty-array-prob (empty-f aps))
  (define get-data-array-prob (data-f aps))
  (define get-size-array-prob (size-f aps))
  (define get-index-array-prob (index-f aps))
  (define set-index-array-prob (index!-f aps))

  (define tparr (make-array-prob (length ti) test-prob-array))
  (define etp (new-sized-array-prob 5))
  (for ([i (in-range 5)])
    (check-eq? (get-index-array-prob etp i) 0.0))
  (check-eq? (get-size-array-prob tparr) 5)
  (check-= (get-index-array-prob tparr 4) (real->prob 42.23) e)
  (set-index-array-prob tparr 3 23.42)
  (set-index-array-prob tparr 100 23.42)
  (get-index-array-prob tparr 100)
  (check-= (get-index-array-prob tparr 3) 23.42 e)

  (check-eq? (get-size-array-prob etp) 5)
  (set-index-array-prob etp 3 42.23)
  (check-= (get-index-array-prob etp 3) 42.23 e)

  (define make-array-array-nat (make-f aans))
  (define new-sized-array-array-nat (new-size-f aans))
  (define empty-array-array-nat (empty-f aans))
  (define get-data-array-array-nat (data-f aans))
  (define get-size-array-array-nat (size-f aans))
  (define get-index-array-array-nat (index-f aans))
  (define set-index-array-array-nat (index!-f aans))
  (define etai (new-sized-array-array-nat 5))
  (set-index-array-array-nat etai 2 eti)
  (define etib (get-index-array-array-nat etai 2))
  (check ptr-equal? etib eti)
  (check-eq? (get-index-array-nat etib 3) 42)

  (define make-array-10double (get-f 'new-sized$array<10.real>))
  (define set-index!-10double (get-f 'set-index!$array<10.real>))
  (define get-index-10double  (get-f 'get-index$array<10.real>))
  (define free-array-10double (get-f 'free-sized$array<10.real>))
  (define a (make-array-10double))
  (set-index!-10double a 4 42.0)
  (check-= (get-index-10double a 4) 42.0 e)
  (free-array-10double a))
