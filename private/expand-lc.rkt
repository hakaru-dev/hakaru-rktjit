#lang racket

(require sham/ast)

(require "ast.rkt"
         "sham-utils.rkt"
         "type-defines.rkt"
         "utils.rkt"
         "prelude.rkt"
         "template-format.rkt"
         "basic-defines.rkt"
         "array-defines.rkt"
         "pair-defines.rkt"
         "probability-defines.rkt")

(provide expand-to-lc)

(define (expand-to-lc mod)
  (define prl (new-prelude))
  (add-defs-prelude! prl (basic-defs))
  (add-defs-prelude! prl (probability-defs))
  (define (get-value v type)
    (match type
      ['nat (nat-value (truncate (inexact->exact v)))]
      ['prob (sham:exp:app (sham:rator:symbol'real2prob)
                           (list (real-value (exact->inexact v))))]
      ['real (real-value (exact->inexact v))]
      ['int (sham:exp:var 'figuroutint)]))

  (define (get-var-sym var-ast)
    (match/values var-ast [((expr-var _ sym _)) sym]))

  (define (get-type type-ast)
    (define type-defs (get-sham-type-define (if-need-pointer type-ast)))
;    (printf "getting types for: ~a\n" (if-need-pointer type-ast))
;    (pretty-display (map print-sham-def type-defs))
;    (newline)
    (add-defs-prelude! prl (reverse type-defs))
    (define type-ref (get-sham-type-ref (car type-defs)))
    (match type-ast
      [`(array ,_)
       (add-defs-prelude! prl (array-defs type-ast))]
      [`(pair ,_ ,_)
       (add-defs-prelude! prl (pair-defs type-ast))]
      [else (void)])
    type-ref)



  (define (ea t rator rands)
    (define trands (map typeof rands))
    (define (build-app rtr (rnds rands))
      (sham:exp:app rtr (map ee rnds)))
    (define sym (expr-intrf-sym rator))
    (cond
      [(simple-rator? sym) (build-app (sham:rator:symbol sym))]
      [(pair-rator? sym) (build-app (get-pair-rator sym t trands))]
      [(array-rator? sym) (build-app (get-array-rator sym t trands))]
      [(math-rator? sym)
       (define-values (defs rtr nrands) (figure-out-math sym rands t trands))
       (add-defs-prelude! prl defs)
       (build-app rtr nrands)]
      [(equal? sym 'superpose-categorical)
       (define-values (defs rtr)
         (build-superpose-categorical (length trands)))
       (add-defs-prelude! prl defs)
       (build-app rtr)]
      ['array-literal
       (define def (build-array-literal (car trands) (length trands)))
       (add-defs-prelude! prl (list def))
       (build-app (sham:rator:symbol (sham:def-id def)))]
      [else (printf "why is this rator not done?: ~a\n" sym)
            (build-app (sham:rator:symbol '?))]))

  (define (ee expr)
    (match expr
      [(expr-app t rator rands) (ea t rator rands)]
      [(expr-var t sym _) (sham:exp:var sym)]
      [(expr-val t v) (get-value v t)]
      [(expr-let t var val body)
       (sham:exp:let (list (get-var-sym var))
                     (list (get-type (typeof var)))
                     (list (ee val))
                     (sham:stmt:void)
                     (ee body))]

      [(expr-block t stmt body)
       (sham:exp:let '() '() '() (es stmt) (ee body))]
      [(expr-if t tst thn els) (sham:exp:void)]
      [else (error (format "not expanding for expr: ~a\n" expr))]))
  (define expand-expr ee)

  (define (for->while index end body)
    (values
     (ee (expr-app 'i1 (expr-intrf '<) (list index end)))
     (sham$block
      (es body)
      (es (stmt-assign
           index
           (expr-app (typeof index) (expr-intrf '+)
                     (list index (expr-val (typeof index) 1))))))))

  (define (es stmt)
    (match stmt
      [(stmt-if tst thn els)
       (sham:stmt:if (ee tst) (es thn) (es els))]
      [(stmt-elets vars vals stmt)
       (sham:stmt:expr
        (sham:exp:let
         (map get-var-sym vars)
         (map (compose get-type typeof) vars)
         (map ee vals)
         (es stmt)
         (sham:exp:void)))]
      [(stmt-for i start end body)
       (define symi (get-var-sym i))
       (define-values (tst b) (for->while i end body))
       (sham:stmt:expr
        (sham:exp:let (list symi)
                      (list (get-type (typeof i)))
                      (list (ee start))
                      (sham:stmt:while tst b)
                      (sham:exp:void)))]
      [(stmt-block stmts) (sham:stmt:block (map es stmts))]
      [(stmt-assign var val) ;;TODO set-index-array
       (sham:stmt:set! (ee var) (ee val))]
      [(stmt-return v) (sham:stmt:return (ee v))]
      [(stmt-void) (sham:stmt:void)]))
  (define expand-stmt es)

  (define (expand-fun p)
    (dprintf #t "expanding function: ~a\n" (car p))
    (match (cdr p)
      [(expr-fun args ret-type b)
       (sham:def:function
        (car p) '() '(AlwaysInline)
        (map get-var-sym args) (map (compose get-type expr-var-type) args)
        (get-type ret-type)
        (expand-stmt b))]))
  (define prog (expand-fun (cons 'prog (expr-mod-main mod))))
  (pretty-display (sham-def->sexp prog))
  (sham:module
   (basic-mod-info)
   (append (cleanup-defs (get-defs-prelude prl))
           (cons prog
                 (map expand-fun (expr-mod-fns mod))))))
