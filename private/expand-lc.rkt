#lang racket

(require sham/private/ast)

(require "ast.rkt")
(require "utils.rkt")

(provide expand-lc)

(define (get-type tast)
  (match tast
    [`(array ,t)
     (symbol-append (symbol-append 'array- t) '-p)]
    [else tast]))

(define op-map
  (make-hash
   '((+ . jit-add-nuw)
     (* . jit-mul-nuw)
     (< . jit-icmp-ult)
     (== . jit-icmp-eq)
     (and . jit-and)
     (or . jit-or)
     (recip . recip-real)
     (not . jit-not))))

(define (get-value v type)
  (match type
    ['nat (ast-exp-ui-value v 'nat)]
    ['prob (ast-exp-fl-value (exact->inexact v) 'prob)]))

(define (nat-value n)
  (get-value n 'nat))

(define (expr-type e)
  (match e
    [(expr-app t _ _) t]
    [(expr-var t _ _) t]
    [(expr-val t _)   t]))

(define (get-rator-sym rator-sym rands)
  (define r-type (get-type (expr-type (car rands))))
  (match rator-sym
    ['index (symbol-append 'index- r-type)]
    ['size(symbol-append 'size- r-type)]
    ['+ (symbol-append 'add- r-type)]
    ['* (symbol-append 'mul- r-type)]
    [else (hash-ref op-map rator-sym rator-sym)]))

(define (fold-stmt body body-type index index-init index-end assign-to fn)
  (define i (ef index))
  (define ti (expr-var-type index))
  (define tmp (gensym^ 'tmp))
  (define tmpi (gensym^ 'tmpi))
  (ast-stmt-let
   tmp body-type (initial-value body-type)
   (ast-stmt-let
    i ti index-init
    (ast-stmt-block
     (ast-stmt-while
      (list (ef i) tmp)
      (list (expr-var-type i) (get-type t))
      (ast-exp-app 'jit-icmp-ult i index-end)
      (ast-stmt-let
       tmpi t (get-value 0 t)
       (ast-stmt-block
        (list
         (expand-fnb b tmpi)
         (fn tmp tmpi)
         (ast-stmt-set!
          i
          (ast-exp-app 'jit-add-nuw i (nat-value 1)))))))
     (ast-stmt-set! assign-to tmp)))))

(define (fold-fn fn)
  (λ (tmp tmpi)
    (ast-stmt-set! tmp (ast-exp-app fn tmp tmpi))))

(define (expand-fnb b to)
  (match b
    [(expr-sum t i start end b)
     (fold-stmt b t (ef i) (ef start) (ef end) to
                (fold-fn (symbol-append 'add- t)))]
    [(expr-let type var val b)
     (ast-stmt-let
      (ef var) (expr-var-type var) (ef val)
      (expand-fnb b to))]
    [(expr-prd t i start end b)
     (fold-stmt
      b t (ef i) (ef start) (ef end) to
      (fold-fn (symbol-append 'mul- t)))]
    [(expr-arr t i end b)
     (fold-stmt
      b t (ef i) (ast-exp-ui-value 0 'nat) (ef end) to
      (λ (tmp tmpi)
        (ast-stmt-exp
         (ast-exp-app
          (string->symbol (format "set-array-~a-at-index" (cadr t)))
          tmp (ef i) tmpi))))]
    [(expr-if t tst thn els)
     (ast-stmt-if (ef tst)
                  (expand-fnb thn to)
                  (expand-fnb els to))]
    [(expr-app t rt rds)
     (ast-stmt-set! to (ef b))]
    [(expr-val t v)
     (ast-stmt-set! to (ef b))]))

(define (ef b)
  (match b
    [(expr-app t rt rds)
     (ast-exp-app (get-rator-sym (ef rt) rds) (map ef rds))]
    [(expr-var t sym o)
     (ast-exp-var sym)]
    [(expr-intr sym)
     (ast-exp-var sym)]
    [(expr-val t v)
     (get-value v t)]
    [else b]))

(define (expand-lc fnps)
  (for/list ([fnp fnps])
    (define fn-name (car fnp))
    (define fn (cdr fnp))
    (define ret (ast-exp-var (gensym^ 'ret)))
    (match fn
      [(expr-fun args ret-type b)
       (ast-function-def
        fn-name '() '(AlwaysInline)
        (map ef args) (map (curry get-type expr-var-type) args)
        (get-type ret-type)
        (ast-stmt-let
         ret (get-type ret-type) (initial-value ret-type) ;;allocates twice for array
         (ast-stmt-block
          (list
           (expand-fnb b ret)
           (ast-stmt-ret ret)))))])))
