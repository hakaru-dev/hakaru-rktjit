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
  (member sym '(struct-literal struct-index)))

(define (get-struct-rator id tresult trands)
  (values (string->symbol
           (match id
             ['index (format struct-get-index-fun-format (get-type-string (car trands)) (length trands))]
             ['make  (format struct-make-fun-format (get-type-string tresult))]
             ['literal  (format struct-literal-fun-format (get-type-string tresult))]))
          (void)))

(define (get-struct-defs tast)
  (define (get-struct-data-types type)
    (match type
      [(sham:type:struct _ _ data-types) data-types]))
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
    (sham:def:function
     (prelude-function-info)
     (get-fun-name struct-literal-fun-format)
     (build-list (length dts) get-vi)
     dts aptref
     (sham:stmt:let
      '(st) (list aptref)
      (list (sham:expr:app
             (sham:rator:symbol (get-fun-name struct-make-fun-format)) '()))
      (sham:stmt:block
       (append (for/list ([i (in-range (length dts))]
                          [dt dts])
                 (sham:stmt:expr
                  (sham$app store! (get-vi i) (sham:expr:gep (sham$var st) (list (sham:expr:ui-value 0)
                                                                                 (sham:expr:ui-value i))))))
               (list (sham:stmt:return (sham$var 'st))))))))
  (define get-indexes
    (for/list ([i (in-range (length dts))]
               [dt dts])
      (sham:def:function
       (prelude-function-info)
       (string->symbol (format struct-get-index-fun-format (sham:def-id atdef) i))
       '(st ind) (list aptref nat) dt
       (sham:stmt:return
        (sham$app load
                  (sham:expr:gep (sham$var 'st)
                                 (list (sham:expr:ui-value 0 nat)
                                       (sham$var i))))))))
  (cons (make-struct) (cons make-literal get-indexes)))
