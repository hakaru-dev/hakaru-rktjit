#lang racket

(require sham/private/ast-utils
         sham/private/jit-utils
         sham/private/ast-info
         sham/private/parameters)
(require ffi/unsafe)
(require "template-format.rkt"
         "type-defines.rkt"
         "basic-defines.rkt"
         "utils.rkt")

(provide array-rator?
         get-array-rator
         array-free array-get-size array-clear array-make array-index array-set!
         array-type build-array-literal)
(define debug-arrays (make-parameter #f))
(define (array-rator? sym)
   (member sym '(empty index size set-index! array-literal
                       free const-size-array-literal clear)))


(define array-type i8*)
(define size-type i64)
(define data-type i64)

(define-sham-function (array-free (p : array-type) : tvoid)
  (free^ p)
  (return-void))

(define-sham-function (array-get-size (p : array-type) : size-type)
 (ret (load (ptrcast p (etype (tptr size-type))))))

(define-sham-function (array-clear (p : array-type) : tvoid)
  (slet^ ([ad (gep (ptrcast p (etype (tptr data-type))) (list (ui64 1))) : array-type])
         (ri^ memset.p0i8.i64 tvoid ad (ui8 0)
              (mul-nuw (intcast (sizeof tprob) (etype size-type))
                       (array-get-size p))
              (ui1 0))
         (svoid))
  (return-void))

(define-sham-function (array-make (size : i64) : array-type)
  (slet^ ([ap (ptrcast (arr-malloc (etype data-type) (add-nuw size (ui64 1)))
                       (etype array-type)) : i8*])
         (store! size (ptrcast ap (etype (tptr size-type))))
         (array-clear ap)
         (ret (ptrcast ap (etype i8*)))))

(define-sham-function (array-index (p : array-type) (n : size-type) : data-type)
  (ret (load (gep (ptrcast p (etype (tptr data-type))) (list (add-nuw n (ui64 1)))))))

(define-sham-function (array-set! (p : array-type) (n : size-type) (v : data-type) : tvoid)
  (store! v (gep (ptrcast p (etype (tptr data-type))) (list (add-nuw n (ui64 1)))))
  (return-void))

(define ((build-general-array-clear arrt) arr)
  (match arrt
    [`(array ,t (size . ,s))
     (let^ ()
           (ri^ memset.p0i8.i64 tvoid
                (ptrcast arr (etype i8*))
                (ui8 0)
                (mul-nuw (intcast (sizeof (get-sham-type t)) (etype tnat))
                         (ui64 s))
                (ui1 0))
           arr)]
    [`(array ,t)
     (let^ () (array-clear arr) arr)]))

(define ((build-general-set-index trands) arr i v)
  (match (first trands)
    [`(array ,t (size . ,s))
     (store! v (gep^ arr (ui64 0) i))]
    [`(array ,t)
     (store! v (gep^ arr (add-nuw i (ui64 1))))]
    [else (error "unknown type for array-set-index" trands)]))

(define ((build-general-get-index trands) arr i)
  (match (first trands)
    [`(array (array ,t (size . ,s1)) (size . ,s))
     (gep^ arr (ui64 0) i)]
    [`(array ,t (size . ,s))
     (load (gep^ arr (ui64 0) i))]
    [`(array ,t)
     (load (gep^ arr (add-nuw i (ui64 1))))]
    [else (error "unknown type for array-get-index" trands)]))

(define ((build-array-literal tresult trands) . args)
  (define data-type (match (clean-measure (first trands))
                      [(or 'real 'prob) f64]
                      [(or 'nat 'int) i64]))
  (let^ ([ap (arr-malloc (etype data-type) (ui64 (length args))) : (tptr data-type)])
        (block (for/list ([arg args]
                          [i (in-range (length args))])
                 (store! arg (gep^ ap (ui64 i)))))
        ap))
(define (get-array-rator rator tresult trands)
  (match rator
    ['set-index!
     (build-general-set-index trands)]
    ['index (build-general-get-index trands)]
    ['size (λ (p) (load (ptrcast p (etype (tptr size-type)))))]
    ['empty array-make]
    ['clear (build-general-array-clear (first trands))]
    ['free (λ (p) (free^ p))]
    ['array-literal (build-array-literal tresult trands)]))

(module+ test
  (require rackunit ;; ffi/unsafe
           )
  (define (make-sized-hakrit-array arr type)
    (define ret (list->cblock (cons (car arr) arr) (rkt-type type)))
    (ptr-set! ret _uint64 0 (length arr))
    ret)
  (define (sized-hakrit-array-size arr) (ptr-ref arr _uint64 0))
  (define (sized-hakrit-array->racket-list ptr type)
    (define size (sized-hakrit-array-size ptr))
    (define lst (cblock->list ptr (rkt-type type) (add1 size)))
    (cdr lst))
  (define (rkt-type t)
    (match t
      ['nat _uint64]
      ['prob _double]
      ['real _double]))

  (parameterize ([compile-options `(dump verify mc-jit)])
    (compile-sham-module!
     (current-sham-module)
     #:opt-level 0))
  (define tarray (sham-app array-make 9))
  (sham-app array-set! tarray 8 42)
  (for ([i (range 9)]) (sham-app array-set! tarray i i))
  (pretty-print (for/list ([i (range 9)]) (sham-app array-index tarray i)))
  (check-equal? (sham-app array-index tarray 8) 8)
  (sham-app array-clear tarray)
  (check-equal? (sham-app array-index tarray 8) 0)

  (define ta (make-sized-hakrit-array (build-list 10 (const 5.0)) 'real))
  (sized-hakrit-array->racket-list ta 'real)
  (sham-app array-clear ta)
  (sized-hakrit-array->racket-list ta 'real)

(define tb (make-sized-hakrit-array (build-list 10 (const 5)) 'nat))
(sized-hakrit-array->racket-list tb 'nat)
;; '(5 5 5 5 5 5 5 5 5 5)
(sham-app array-clear tb)
(sized-hakrit-array->racket-list tb 'nat)
;; '(0 0 0 0 0 0 0 0 0 0)

  ;; (sham-app array-free tarray)
  )

#|
(define (simple-array-defs tast)
  (match-define `(array ,t) tast)
  (define st (get-sham-type tast))
  (define atp (tptr st))
  (define dt  (get-sham-type t))
  (define dtp (tptr dt))
  (common-function-info (prelude-function-info))
  (define make-array
    (sham-function
     (,(get-function-id array-make-format tast)
      (size : tnat) (data : dtp)) : atp
     (slet^ ([ap (malloc^ (etype st)) : atp])
            (store! size (get-struct-field ap 0))
            (store! data (get-struct-field ap 1))
            (ret ap))))
  (define free-array
    (sham-function
     (,(get-function-id array-free-format tast)
      (p : atp)) : tvoid
     (free^ p)
     ret-void))

  (define make-empty-array
    (sham-function
     (,(get-function-id array-make-empty-format tast)
      (size : tnat)) : atp
     (slet^ ([dp (arr-malloc (etype dt) size) : dtp])
            (ret (make-array size dp)))))
  (define clear-array
    (sham-function
     (,(get-function-id array-clear-format tast)
      (p : atp)) : tvoid
     (ri^ memset.p0i8.i64 tvoid (ptrcast (get-data p) (etype (tptr i8))) (ui8 0)
          (mul (get-size p) (sizeof dt)) (ui1 0))
     ret-void))


  (define get-size
    (sham-function
     (,(get-function-id array-get-size-format tast)
      (p : atp)) : tnat
     (ret (load (get-struct-field p 0)))))
  (define set-size
    (sham-function
     (,(get-function-id array-set-size-format tast)
      (p : atp) (size : tnat)) : tvoid
     (store! size (get-struct-field p 0))
     ret-void))

  (define get-data
    (sham-function
     (,(get-function-id array-get-data-format tast)
      (p : atp)) : dtp
     (ret (load (get-struct-field p 1)))))
  (define set-data
    (sham-function
     (,(get-function-id array-set-data-format tast)
      (p : atp) (data : dtp)) : tvoid
     (store! data (get-struct-field p 1))
     ret-void))

  (define get-at-index
    (sham-function
     (,(get-function-id array-get-index-format tast)
      (p : atp) (n : tnat)) : dt
     (ret (load (gep (load (get-struct-field p 1))
                     (list n))))))
  (define set-at-index
    (sham-function
     (,(get-function-id array-set-index-format tast)
      (p : atp) (n : tnat) (v : dt)) : tvoid
     (store! v
             (gep (load (get-struct-field p 1))
                  (list n)))
     ret-void))

  (list make-array free-array make-empty-array clear-array
        get-size set-size get-data set-data get-at-index set-at-index))

#;(module+ test
  (require rackunit
           "../../../sham/private/jit-utils.rkt")

  (define simple-test-module (create-empty-sham-module "array-test-mdoule"))
  (current-sham-module simple-test-module)
  (define dfs (simple-array-defs '(array nat)))
  (map (curry add-to-sham-module! (current-sham-module)) dfs)
  (parameterize ([compile-options `(pretty dump verify mc-jit)])
    (compile-sham-module!
     (current-sham-module)
     #:opt-level 3))

  (match-define (list make-array free-array make-empty clear
                      get-size set-size get-data set-data get-index set-index)
    dfs)

  (define tarr (sham-app make-array 10 #f))
  (define  mem (malloc 20 _uint))
  (check-equal? (sham-app get-size tarr) 10)
  (sham-app set-size tarr 20)
  (check-equal? (sham-app get-size tarr) 20)
  (sham-app set-data tarr mem)
  (check-equal? (sham-app get-data tarr) mem)
  (sham-app set-index tarr 10 42)
  (check-equal? (sham-app get-index tarr 10) 42)

  (define ea (sham-app make-empty 10))
  (sham-app clear ea)
  (check-equal? (sham-app get-index ea 5) 0))

#|
;;tast be expanded
(define (const-array-defs tast)
  ;; (pretty-print tast)
  (define (get-array-data-type at)
    (match at
      [(sham:type:array _ t _) t]))
  (match-define `(array ,dtype (size . ,size)) tast)

  (define nested-array
    (and (pair? dtype)
         (equal? (car dtype) 'array)))
  ;; (printf "const-array-defs; ~a, ~a\n" tast nested-array)


  (define nat type-nat-ref)
  (define nat* (sham:type:pointer nat))

  ;; all defines, main define, main type, main type ref
  (define-values (atdefs atdef at atref) (defs-def-t-tref tast))
  (define-values (aptdefs aptdef apt aptref) (defs-def-t-tref `(pointer ,tast)))
  (define-values (adtdefs adtdef adt adtref) (defs-def-t-tref dtype))
  (define-values (apdtdefs apdtdef apdt apdtref) (defs-def-t-tref `(pointer ,dtype)))

  (define (get-size tast)
    (match tast
      [`(array ,elem (size . ,c)) (* c (get-size elem))]
      [else 1]))
  (define total-size (get-size tast))

  (define (get-fun-name frmt)
    (string->symbol (format frmt (sham:def-id atdef))))

  (define (make-array)
    (sham$def-function
     (prelude-function-info)
     ((get-fun-name new-size-array-fun-format)) aptref
     (sham$return (sham$app bitcast (sham$app 'malloc (sham$etype atref)) (sham$etype aptref)))))

  (define (clear-array)
    (sham$def-function
     (prelude-function-info)
     ((get-fun-name clear-size-array-fun-format)
      ('arr aptref)) aptref
     ;; (sham:stmt:expr
     ;;  (sham:expr:app
     ;;   (sham:rator:racket
     ;;    (gensym 'clear-info)
     ;;    (λ (b) (printf "clear-info\n") (printf "clear array ~a: ~a\n"
     ;;                                           tast (cblock->list b _uint64 total-size)))
     ;;    (sham:type:function (list (sham:type:pointer (sham:type:ref 'i8)))
     ;;                        (sham:type:ref 'void)))
     ;;   (list (sham$var arr))))
     (sham:stmt:expr
      (sham:expr:app
       (sham:rator:intrinsic 'llvm.memset.p0i8.i64 (sham:type:ref 'void))
       (list (sham$app ptrcast (sham$var 'arr) (sham:expr:type (sham:type:pointer (sham:type:ref 'i8))))
             (sham$app intcast (nat-value 0) (sham:expr:type (sham:type:ref 'i8)))
             (sham$app intcast (sham:expr:sizeof at) (sham$etype nat))
             (sham$app intcast (nat-value 0) (sham$etype i32))
             (sham$app intcast (nat-value 0) (sham$etype i1)))))
     (sham:stmt:return (sham$var arr))))
  (define (free-array)
    (sham$def-function
     (prelude-function-info)
     ((get-fun-name free-size-array-fun-format)
      ('arr aptref)) (sham:type:ref 'void)
     (sham:stmt:expr
      (sham:expr:app (sham:rator:symbol 'free) (list (sham$var 'arr))))
     (sham:stmt:return (sham:expr:void))))

  (define (get-array-size)
    (sham$def-function
     (prelude-function-info)
     ((get-fun-name get-array-size-fun-format)
      ('arr aptref)) nat
     (sham:stmt:return (sham:expr:ui-value size nat))))

  (define (get-index)
    (sham:def:function
     (prelude-function-info)
     (get-fun-name get-index-fun-format)
     '(arr ind) (list aptref nat) (if nested-array apdtref adtref)
     (sham$block
      ;; (sham:stmt:expr
      ;;  (sham:expr:app
      ;;   (sham:rator:racket
      ;;    (gensym 'get-index-info)
      ;;    (λ (a b) (printf "get-index-info\n")
      ;;       (printf "get-index ~a: ~a ~a\n"
      ;;               tast a (cblock->list b _uint64 total-size))
      ;;       (when (> a total-size)
      ;;         (error 'out-of-index)))
      ;;    (sham:type:function (list nat (sham:type:pointer (sham:type:ref 'i8)))
      ;;                        (sham:type:ref 'void)))
      ;;   (list (sham$var ind) (sham$var arr))))
      (sham:stmt:return
       (let [(ptr (sham:expr:gep (sham$var 'arr)
                                 (list (nat32-value 0) (sham$var ind))))]
         (if nested-array ptr (sham:expr:app (sham$rator 'load) (list ptr))))))))

  (define (set-index)
    (sham:def:function
     (prelude-function-info)
     (get-fun-name set-index-fun-format)
     '(arr ind v) (list aptref nat (if (pair? dtype) apdtref adtref)) (sham:type:ref 'void)
     (sham$block
      (sham:stmt:expr (sham$app store!
                                (if (pair? dtype) (sham$app 'load v) (sham$var v))
                                (sham:expr:gep (sham$var 'arr) (list (nat32-value 0) (sham$var ind)))))
      (sham:stmt:return (sham:expr:void)))))
  (append
   (reverse adtdefs)
   (reverse apdtdefs)
   (reverse atdefs)
   (reverse aptdefs)
   (list type-nat-def)
   (list
    (make-array)
    (clear-array)
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


#;(module+ test
  (require rackunit
           sham/jit
           ffi/unsafe)

  (define defs
    (append  (append-map const-array-defs
                         `((array (array nat (size . 10)) (size . 10))
                           (array real (size . 10))))
            (append-map array-defs
                        `((array nat)
                          (array real)
                          (array prob)
                          (array (array nat))))
            (list (build-array-literal `(array real) 3))
            ))



  (pretty-print (map print-sham-def defs))
  (define mod
    (sham:module
     (basic-mod-info)  defs))

  (define cmod (compile-module mod))
  ;; (jit-dump-module cmod)
  (optimize-module cmod)
   (jit-dump-module cmod)
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
|#
|#
