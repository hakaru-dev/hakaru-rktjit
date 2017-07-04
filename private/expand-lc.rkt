#lang racket

(require sham/private/ast)

(require "ast.rkt")
(require "utils.rkt")

(provide expand-to-lc)

(define (get-type tast)
  (match tast
    [`(array ,t)
     (symbol-append (symbol-append 'array- t) '-p)]
    [else tast]))
(define (array-type t)
  (match t
    [`(array ,t) t]))
(define op-map
  (make-hash
   '((< . jit-icmp-ult)
     (== . jit-icmp-eq)
     (and . jit-and)
     (or . jit-or)
     (not . jit-not))))

(define (get-value v type)
  (match type
    ['nat (ast-exp-ui-value v 'nat)]
    ['prob (ast-exp-app 'real2prob
                        (list (ast-exp-fl-value (exact->inexact v) 'real)))]
    ['real (ast-exp-fl-value (exact->inexact v) 'real)]))

(define (nat-value n)
  (get-value n 'nat))

(define (expr-type e)
  (typeof e))

(define (get-rator-sym rator rands)
  (match rator
    [(expr-intrf s) s]
    [(expr-intr s)
     (define r-type (get-type (expr-type (car rands))))
     (match s
       ['index (symbol-append 'index- r-type)]
       ['size (symbol-append 'size- r-type)]
       ['recip (symbol-append 'recip- r-type)]
       ['+ (string->symbol (format "add-~a-~a" (length rands) r-type))]
       ['* (string->symbol (format "mul-~a-~a" (length rands) r-type))]
       [else (hash-ref op-map s s)])]))

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

(define (fold-stmt body body-type ret-type init-value index index-init index-end assign-to fn)
  (define i index)
  (define ti 'nat)
  (define tmp (gensym^ 'tmp))
  (define tmpi (gensym^ 'tmpi))
  (ast-stmt-let
   tmp (get-type ret-type) init-value
   (ast-stmt-let
    i (get-type ti) index-init
    (ast-stmt-block
     (list
      (ast-stmt-while
       (list i tmp)
       (list 'nat (get-type body-type))
       (ast-exp-app 'jit-icmp-ult (list i index-end))
       (ast-stmt-let
        tmpi (get-type body-type) (get-value 0 body-type)
        (ast-stmt-block
         (list
          (expand-fnb body tmpi)
          (fn tmp tmpi)
          (ast-stmt-set!
           i
           (ast-exp-app 'jit-add-nuw (list i (nat-value 1))))))))
      (ast-stmt-set! assign-to tmp))))))

(define (fold-fn fn)
  (λ (tmp tmpi)
    (ast-stmt-set! tmp (ast-exp-app fn (list tmp tmpi)))))
(define (wrap-with-exp var type val body)
  (ast-stmt-let
   var type '#%void
   (ast-stmt-block
    (list
     (expand-fnb val var)
     body))))

(define (get-var-ast v)
  (match v
    [(expr-var t sym o)
     (ast-exp-var sym)]
    [(expr-intr sym)
     (ast-exp-var sym)]))

(define (expand-fnb b to)
  (match b
    [(expr-let type var val b)
     (define var-ast (get-var-ast var))
     (ast-stmt-let
      var-ast (get-type (expr-var-type var)) '#%void 
      (ast-stmt-block
       (list
        (expand-fnb val var-ast)
        (expand-fnb b to))))]
    [(expr-sum t i start end b)
     (define se (ast-exp-var (gensym^ 'se)))
     (wrap-with-exp
      se 'nat end
      (fold-stmt
       b t t (value 0 t) (get-var-ast i) (expand-exp start) se to
       (fold-fn (symbol-append 'add-2- t))))]
    [(expr-prd t i start end b)
     (define pe (ast-exp-var (gensym^ 'pe)))
     (wrap-with-exp
      pe 'nat end
      (fold-stmt
       b t t (value 1 t) (expand-exp i) (expand-exp start) pe to
       (fold-fn (symbol-append 'mul-2- t))))]
    [(expr-arr t i end b)
     (define ae (ast-exp-var (gensym^ 'ae)))
     (wrap-with-exp
      ae 'nat end
      (fold-stmt
       b (array-type t) t (empty-array t ae)
       (expand-exp i) (ast-exp-ui-value 0 'nat) ae to
       (λ (tmp tmpi)
         (ast-stmt-exp
          (ast-exp-app
           (string->symbol (format "set-array-~a-at-index" (cadr t)))
           (list tmp (expand-exp i) tmpi))))))]
    [(expr-if t tst thn els)
     (define ife (ast-exp-var (gensym^ 'ift)))
     (wrap-with-exp ife (expr-type (get-type tst)) tst
                    (ast-stmt-if ife (expand-fnb thn to) (expand-fnb els to)))]
    [(expr-app t rt rds)
     (define rands-ast (for/list ([rd rds]) (ast-exp-var (gensym^ 'rd))))
     (for/fold [(body (ast-stmt-set! to (ast-exp-app (get-rator-sym rt rds) rands-ast)))]
               [(r rands-ast)
                (rd rds)]
       (wrap-with-exp r (get-type (expr-type rd)) rd  body))]
    [(expr-val t v) (ast-stmt-set! to (get-value v t))]
    [(expr-var type sym orig) (ast-stmt-set! to sym)]))

(define (expand-exp b)
  (match b
    [(expr-app t rt rds)
     (ast-exp-app (get-rator-sym rt rds) (map expand-exp rds))]
    [(expr-var t sym o)
     (ast-exp-var sym)]
    [(expr-intr sym)
     (ast-exp-var sym)]
    [(expr-val t v)
     (get-value v t)]
    [else (error "cannot expand expression" b)]))

(define (expand-stmt stmt)
  (match stmt
    ([stmt-lets vars bstmt]
     (for/fold [(stmt (expand-stmt bstmt))]
               [(var vars)]
       (ast-stmt-let (expand-exp var) (get-type (typeof var)) (ast-exp-void-value)
                     stmt)))
    ([stmt-if tst thn els]
     (ast-stmt-if (expand-exp tst)
                  (expand-stmt thn)
                  (expand-stmt els)))
    ([stmt-block stmts]
     (ast-stmt-block (map expand-stmt stmts)))
    ([stmt-assign (expr-app t (expr-intr 'index) (list arr ind)) val]
     (ast-stmt-exp (ast-exp-app (string->symbol (format "set-array-~a-at-index" (cadr (typeof arr))))
                   (map expand-exp (list arr ind val)))))
    ([stmt-assign var val]
     (ast-stmt-set! (expand-exp var) (expand-exp val)))
    ([stmt-void]
     (ast-stmt-exp (ast-exp-void-value)))
    ([stmt-for i start end body]
     (define end-sym (gensym^ 'end))
     (ast-stmt-let
      (expand-exp i) (typeof i) (expand-exp start)
      (ast-stmt-let
       end-sym (typeof i) (expand-exp end)
       (ast-stmt-while
        (list (expand-exp i)) (list (get-type (typeof i)))
        (ast-exp-app 'jit-icmp-ult (list (expand-exp i) end-sym))
        (ast-stmt-block
         (list
          (expand-stmt body)
          (ast-stmt-set! (expand-exp i) (ast-exp-app 'jit-add-nuw (list (expand-exp i) (nat-value 1))))))))))
    ([stmt-return val]
     (ast-stmt-ret (expand-exp val)))))

(define (expand-to-lc mod)
  (ast-module
   (cons
    (match (expr-mod-main mod)
      [(expr-fun args ret-type b)
       (define ret (ast-exp-var (gensym^ 'ret)))
       (ast-function-def
        'main '() '(AlwaysInline)
        (map expand-exp args) (map (compose get-type expr-var-type) args)
        (get-type ret-type)
        (expand-stmt b)
        ;; (ast-stmt-let
        ;;          ret (get-type ret-type) (initial-value ret-type) ;;allocates twice for array
        ;;          (ast-stmt-block
        ;;           (list
        ;;            (expand-fnb b ret)
        ;;            (ast-stmt-ret ret))))
        )]) 
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
             (ast-stmt-ret ret)))))])))))
