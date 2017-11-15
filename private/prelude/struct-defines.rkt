#lang racket

(require sham
         (submod sham/ast utils))
(require "template-format.rkt"
         "type-defines.rkt"
         "basic-defines.rkt"
         "utils.rkt")

(provide get-struct-defs
         struct-rator?
         get-struct-rator)

(define (struct-rator? sym)
  (or (member sym '(make-struct struct-literal))
      (is-struct-index? sym)))

(define (is-struct-index? sym)
  (string-prefix? (symbol->string sym) "struct-index"))

(define (get-struct-rator id tresult trands)
  (cond
    [(is-struct-index? id) (build-struct-index id tresult trands)]
    [else
     (values
      (sham:rator:symbol
       (string->symbol
        (match id
          ['make-struct  (format struct-make-fun-format (get-type-string tresult))]
          ['struct-literal  (format struct-literal-fun-format (get-type-string tresult))])))
      (void))]))

(define (get-struct-data-types type)
  (match type
    [(sham:type:struct _ _ data-types) data-types]))

(define (get-struct-defs tast)
  (define-values
    (atdefs atdef at atref) (defs-def-t-tref tast))
  (define-values
    (aptdefs aptdef apt aptref) (defs-def-t-tref `(pointer ,tast)))
  (define nat type-nat-ref)
  (define (get-fun-name frmt)
    (string->symbol (format frmt (sham:def-id atdef))))

  (define dts (get-struct-data-types at))
  (define (make-struct)
    (sham:def:function
     (prelude-function-info)
     (get-fun-name struct-make-fun-format)
     '() '() aptref
     (sham:stmt:return (sham$app bitcast
                                 (sham$app malloc (sham:expr:type atref))
                                 (sham:expr:type aptref)))))

  (define (make-literal)
    (define nat (sham:type:ref 'nat))
    (sham:def:function
     (prelude-function-info)
     (get-fun-name struct-literal-fun-format)
     (build-list (length dts) get-vi)
     dts aptref
     (sham:stmt:let
      '(st) (list aptref)
      (list (sham:expr:app (sham:rator:symbol (get-fun-name struct-make-fun-format)) '()))
      (sham$block
       (append ;; (for/list ([i (in-range (length dts))])
        ;;   (sham:stmt:expr
        ;;    (sham$app store! (get-vi i) (sham:expr:gep (sham$var st) (list (sham:expr:ui-value 0 nat)
        ;;                                                                   (sham:expr:ui-value i nat))))))
        (for/list ([i (in-range (length dts))])
          (sham:stmt:expr (sham$app store! (sham$var (get-vi i)) (get-struct-field 'st i))))
        (list (sham:stmt:return (sham$var 'st))))))))
  (append
   (list type-nat-def)
   (reverse atdefs)
   (reverse aptdefs)
   (list (make-struct) (make-literal))))

(define (build-struct-index id tresult trands)
  ;(printf "building struct index: ~a, ~a, ~a\n" id tresult trands)
  (define i (string->number (second (string-split (symbol->string id) "."))))
  (define-values (atdefs atdef at atref) (defs-def-t-tref (car trands)))
  (define-values (aptdefs aptdef apt aptref) (defs-def-t-tref `(pointer ,(car trands))))
  (define dts (get-struct-data-types at))
  (define-values (dtdefs dtdef dt dtref) (defs-def-t-tref `(pointer ,tresult)))
  (define nat type-nat-ref)
  (define fn-sym (string->symbol (format (string-append struct-get-index-fun-format "." (number->string i))
                                         (sham:def-id atdef))))
  (values
   (sham:rator:symbol fn-sym)
   (sham:def:function
    (prelude-function-info)
    fn-sym
    `(st) (list aptref) (vector-ref (list->vector dts) i)
    (sham:stmt:return
     (sham$app load (get-struct-field 'st i))))))

(module+ test
  (require rackunit
           sham/jit
           ffi/unsafe
           "../../utils.rkt")
  (define-values (rtr def) (build-struct-index 'index.1 '(array real) `((struct-type ((array real) real)))))
  (define defs (append (get-struct-defs `(struct-type ((array real) real)))
                       (list def)))


  (pretty-print (map print-sham-def defs))
  (define mod
    (sham:module
     (basic-mod-info) defs))

  (define cmod (compile-module mod))
  (jit-dump-module cmod)
  (jit-verify-module cmod)
  (initialize-jit! cmod))
