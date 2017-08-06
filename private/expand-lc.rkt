#lang racket

(require sham/private/ast)

(require "ast.rkt")
(require "utils.rkt")

(provide expand-to-lc)

(define (get-type tast)
  (match tast
    [`(array ,t) #:when (symbol? t)
     (symbol-append (symbol-append 'array- t) '-p)]
    [`(array ,t) (symbol-append 'array- (get-type t))]
    [`(measure ,t) (symbol-append t 'm)]
    [(? symbol?) tast]
    ;; [else tast]
    ))

(define op-map
  (make-hash
   '((< . icmp-ult)
     (== . icmp-eq))))

(define (get-value v type)
  (match type
    ['nat (nat-value v)]
    ['prob (sham:exp:app (sham:rator:symbol'real2prob)
                        (list (real-value (exact->inexact v))))]
    ['real (real-value v)]))

(define (get-rator-sym t rator rands)
  (sham:rator:symbol
   (match rator
     [(expr-intrf 'empty)
      (string->symbol (format "empty-~a-zero" (get-print-type t)))]
     [(expr-intrf s) s]

     [(expr-intr s)
      (define r-type (get-print-type (expr-type (car rands))))
      (match s
        ['index (string->symbol (format "index-~a-p" r-type))]
        ['size  (string->symbol (format "size-~a-p" r-type))]
        ['recip (symbol-append 'recip- r-type)]
        ['+ (string->symbol (format "add-~a-~a" (length rands) r-type))]
        ['* (string->symbol (format "mul-~a-~a" (length rands) r-type))]
        [else (hash-ref op-map s s)])])))

(define (initial-value type)
  (match type
    ['prob (get-value 0.0 'prob)]
    ['nat  (get-value 0   'nat)]
    ['real (get-value 0.0 'real)]
    [`(array ,t) (get-value 0 'nat)]))
(define (value v type)
  (match type
    ['prob (get-value (exact->inexact v) 'prob)]
    ['nat  (get-value (truncate (inexact->exact v)) 'nat)]
    ['real (get-value (exact->inexact v) 'real)]))
(define (empty-array type size)
  (ast-exp-app (string->symbol (format "empty-array-~a" (cadr type))) (list (expand-exp size))))

(define (expand-to-lc mod)
  (define extra-funs (box '()))
  (define (add-fun f)
    (set-box extra-funs (cons f (unbox extra-funs))))
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
       (ast-stmt-exp (ast-exp-void-value))]

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
  (ast-module
   (cons
    (match (expr-mod-main mod)
      [(expr-fun args ret-type b)
       (define ret (ast-exp-var (gensym^ 'ret)))
       (sham:def:function
        'main '() '(AlwaysInline)
        (map get-args args) (map (compose get-type expr-var-type) args)
        (get-type ret-type)
        (expand-stmt b))]) 
    (for/list ([fnp (expr-mod-fns mod)])
      (define fn-name (car fnp))
      (define fn (cdr fnp))
      (define ret (ast-exp-var (gensym^ 'ret)))
      (match fn
        [(expr-fun args ret-type b)
         (ast-function-def
          fn-name '() '(AlwaysInline)
          (map expand-exp args) (map (compose get-type expr-var-type) args)
          (get-type ret-type)
          (ast-stmt-let
           ret (get-type ret-type) (initial-value ret-type) ;;allocates twice for array
           (ast-stmt-block
            (list
             (expand-fnb b ret)
             (ast-stmt-ret ret)))))]))
    )))
