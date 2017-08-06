#lang racket

(require sham/ast)

(require "ast.rkt"
         "sham-utils.rkt"
         "utils.rkt")

(provide expand-to-lc)

(define (get-var-sym var)
  (match var
    [(expr-var t sym o)
     sym]))

(define (get-sham-type t)
  (match t
    [`(array ,typ) (sham:type:pointer (sham:type:ref typ))]
    [(? symbol?) (sham:type:ref t)]))

(define (get-sham-var v)
  (sham:exp:var (get-var-sym v)))
(define (get-type tast)
  (match tast
    [`(array ,t) #:when (symbol? t)
     (symbol-append (symbol-append 'array- t) '-p)]
    [`(array ,t) (symbol-append 'array- (get-type t))]
    [`(measure ,t) (symbol-append t 'm)]
    [(? symbol?) tast]))

(define op-map
  (make-hash
   '((< . icmp-ult)
     (== . icmp-eq))))

(define (get-value v type)
  (match type
    ['nat (nat-value (truncate (inexact->exact v)))]
    ['prob (sham:exp:app (sham:rator:symbol'real2prob)
                        (list (real-value (exact->inexact v))))]
    ['real (real-value (exact->inexact v))]))

(define (get-rator-sym t rator rands)
  (sham:rator:symbol
   (match rator
     [(expr-intrf 'empty)
      (string->symbol (format "empty-~a" (get-print-type t)))]
     [(expr-intrf s) s]
     [(expr-intr s)
      (define r-type (get-print-type (typeof (car rands))))
      (match s
        ['index (string->symbol (format "index-~a" r-type))]
        ['size  (string->symbol (format "size-~a" r-type))]
        ['recip (symbol-append 'recip- r-type)]
        ['+ (string->symbol (format "add-~a-~a" (length rands) r-type))]
        ['* (string->symbol (format "mul-~a-~a" (length rands) r-type))]
        [else (hash-ref op-map s s)])])))

(define (expand-to-lc mod)
  (define (expand-exp b (env (make-immutable-hash)))
    (match b
      [(expr-app t rt rds)
       (sham:exp:app (get-rator-sym t rt rds) (map (curryr expand-exp env) rds))]
      [var #:when (hash-has-key? env var)
           (hash-ref env var)]
      [(expr-var t sym o)
       (sham:exp:var sym)]
      [(expr-val t v)
       (get-value v t)]
      [(expr-let t var val b)
       (expand-exp b (hash-set env var (expand-exp val env)))]
      [(expr-val t v) (get-value v t)]
      [else (error "cannot expand expression" b)]))
  (define (expand-stmt stmt)
    (match stmt
      [(stmt-lets vars bstmt)
       (sham:stmt:let
        (map get-var-sym vars)
        (map (compose get-sham-type typeof) vars)
        (map (const (sham:exp:void-value)) (in-range (length vars)))
        (expand-stmt bstmt))]

      [(stmt-if tst thn els)
       (sham:stmt:if (expand-exp tst)
                     (expand-stmt thn)
                     (expand-stmt els))]

      [(stmt-block stmts)
       (sham:stmt:block (map expand-stmt stmts))]

      [(stmt-assign (expr-app t (expr-intr 'index) (list arr ind)) val)
       (sham:stmt:exp
        (sham:exp:app
         (sham:rator:symbol
          (string->symbol
           (format "set-index-in-" (get-print-type (typeof arr)))))
         (map expand-exp (list arr ind val))))]

      [(stmt-assign var val)
       (sham:stmt:set! (get-sham-var var) (expand-exp val))]
      [(stmt-void)
       (sham:stmt:exp (sham:exp:void-value))]

      ([stmt-for i start end body]
       (define end-sym (gensym^ 'end))
       (sham:stmt:let
        (list (get-var-sym i) end-sym)
        (list (get-sham-type (typeof i)) (get-sham-type (typeof i)))
        (list (expand-exp start) (expand-exp end))
        (sham:stmt:while
         (sham:exp:app (sham:rator:symbol 'icmp-ult)
                       (list (expand-exp i) (sham:exp:var end-sym)))
         (sham:stmt:block
          (list
           (expand-stmt body)
           (sham:stmt:set! (get-sham-var i)
                           (sham:exp:app (sham:rator:symbol 'add-nuw)
                                         (list (get-sham-var i)
                                               (nat-value 1)))))))))
      [(stmt-return val)
       (sham:stmt:return (expand-exp val))]))
  (define (expand-fun f)
    (match f
      [(expr-fun args ret-type b)
       (sham:def:function
        'main '() '(AlwaysInline)
        (map get-var-sym args) (map (compose get-type expr-var-type) args)
        (get-type ret-type)
        (expand-stmt b))]))
  (sham:module
   '()
   (cons (expand-fun (expr-mod-main mod))
         (map expand-fun (expr-mod-fns mod)))))
