#lang racket

(require sham/ast)

(require "ast.rkt"
         "sham-utils.rkt"
         "utils.rkt")

(provide expand-to-lc)

(define (expand-to-lc mod)
  (define prelude-defines (make-hash))
  (define (add-to-prelude defn)
    (hash-set! prelude-defines defn)
    (set-box! prelude-defines (cons defn ((unbox prelude-defines)))))
  (define (get-rator-sym t rator rands)
    (sham:rator:symbol
     (match rator
       [(expr-intrf 'empty)
        (string->symbol (format "empty-~a" (get-print-type t)))]
       [(expr-intrf 'cons)
        (string->symbol (format "make-~a" (get-print-type t)))]
       [(expr-intrf s) s]
       [(expr-intr s)
        (define rand-type (typeof (car rands)))
        (define r-type (get-print-type rand-type))
        (match s
          ['index (string->symbol (format "index-~a" r-type))]
          ['size  (string->symbol (format "size-~a" r-type))]
          ['recip (symbol-append 'recip- r-type)]
          ['+ (string->symbol (format "add-~a-~a" (length rands) r-type))]
          ['* (string->symbol (format "mul-~a-~a" (length rands) r-type))]
          [else (hash-ref op-map s s)])])))
  (define (get-var-sym var)
    (match var
      [(expr-var t sym o) sym]))
  (define (get-sham-var v) (sham:exp:var (get-var-sym v)))

  (define (get-sham-type tast)
    (match tast
      [`(array ,t) (sham:type:ref (get-print-type `(* ,tast)))]
      [`(pair ,t1 ,t2) (sham:type:ref (get-print-type `(* ,tast)))]
      [`(measure ,t) (sham:type:ref (get-print-type tast))]
      [(? symbol?) (sham:type:ref (get-print-type tast))]))

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
  (define (expand-exp b (env (make-immutable-hash)))
    ;; (printf "expanding expr\n") (display-expr b)
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
    ;; (printf "expanding-stmt: \n") (display-stmt stmt)
    (match stmt
      [(stmt-lets vars bstmt)
       (sham:stmt:let
        (map get-var-sym vars)
        (map (compose get-sham-type typeof) vars)
        (make-list (length vars) (sham:exp:void-value))
        (expand-stmt bstmt))]

      [(stmt-if tst thn els)
       (sham:stmt:if (expand-exp tst)
                     (expand-stmt thn)
                     (expand-stmt els))]

      [(stmt-block stmts)
       (sham:stmt:block (map expand-stmt stmts))]

      [(stmt-assign (expr-app t (expr-intr 'index) (list arr ind)) val)
       (sham:stmt:exp-stmt
        (sham:exp:app
         (sham:rator:symbol
          (string->symbol
           (format "set-index-in-~a" (get-print-type (typeof arr)))))
         (map expand-exp (list arr ind val)))
        (sham:stmt:void))]

      [(stmt-assign var val)
       (sham:stmt:set! (get-sham-var var) (expand-exp val))]
      [(stmt-void)
       (sham:stmt:void)]

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
  (define (expand-fun p)
    (printf "expanding function: ~a\n" (car p))
    (match (cdr p)
      [(expr-fun args ret-type b)
       (sham:def:function
        (car p) '() '(AlwaysInline)
        (map get-var-sym args) (map (compose get-sham-type expr-var-type) args)
        (get-sham-type ret-type)
        (expand-stmt b))]))
  (sham:module
   '()
   (cons (expand-fun (cons 'main (expr-mod-main mod) ))
         (append (map expand-fun (expr-mod-fns mod))
                 ;; (unbox prelude-defines)
                 ))))
