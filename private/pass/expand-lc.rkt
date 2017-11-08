#lang racket

(require sham/ast
         sham/jit)

(require "../ast.rkt"
         "prelude.rkt"
         "utils.rkt"
         "../utils.rkt")

(provide expand-to-lc)

(define (expand-to-lc mod)
  (define prl (new-prelude))
  (add-basic&probability-defs prl)

  (define (get&add-type type-ast)
    (define-values (ref defs) (get-defs&ref-type type-ast))
    (add-defs-prelude! prl defs)
    ref)

  (define (ea tresult rator rands)
    (define trands (map typeof rands))
    (define-values (rtr def) (get-rator (expr-intrf-sym rator) tresult trands))
    (unless (void? def)
      (add-defs-prelude! prl (list def)))
    (sham:expr:app rtr (map ee rands)))

  (define (ee expr)
    (match expr
      [(expr-app t rator rands) (ea t rator rands)]
      [(expr-var t sym _) (sham:expr:var sym)]
      [(expr-val t v) (get-value v t)]
      [(expr-lets types vars vals stmt body)
       (sham:expr:let (map expr-var-sym vars)
                      (map (compose get&add-type typeof) vars)
                      (map ee vals)
                      (es stmt)
                      (ee body))]
      [(expr-if t tst thn els)
       (define v (gensym^ 'if))
       (define ev (expr-var t v '_))
       (sham:expr:let (list v) (list (get&add-type t)) (list (sham:expr:void))
                      (es (stmt-if tst
                                   (stmt-assign ev thn)
                                   (stmt-assign ev els)))
                      (ee ev))]
      [else (error (format "not expanding for expr: ~a\n" (pe expr)))]))

  (define (for->while index start end body)
    (sham:stmt:expr
     (sham:expr:let
      (list (expr-var-sym index)) (list (get&add-type (typeof index))) (list (ee start))
      (sham:stmt:while
       (ee (expr-app 'i1 (expr-intrf '<) (list index end)))
       (sham:stmt:block
        (list (es body)
              (es (stmt-assign index (expr-app (typeof index) (expr-intrf '+)
                                               (list index (expr-val (typeof index) 1))))))))
      (sham:expr:void))))

  (define (es stmt)
    (match stmt
      [(stmt-if tst thn els) (sham:stmt:if (ee tst) (es thn) (es els))]
      [(stmt-expr (stmt-void) e)
       (sham:stmt:expr (ee e))]
      [(stmt-expr s e)
       (sham:stmt:block (list (es s) (sham:stmt:expr (ee e))))]
      [(stmt-for i start end body) (for->while i start end body)]
      [(stmt-block stmts) (sham:stmt:block (map es stmts))]
      [(stmt-assign var val) (sham:stmt:set! (ee var) (ee val))]
      [(stmt-return v) (sham:stmt:return (ee v))]
      [(stmt-void) (sham:stmt:void)]
      [else (error (format "not expanding stmt: ~a\n" (ps stmt)))]))

  (define (mod-fun-info) (void)) ;;TODO
  (define (mod-info) (void))

  (define (expand-fun fp)
    (define fname (car fp))
    (match (cdr fp)
      [(expr-fun args ret-type b)
       (dtprintf "expanding-function: ~a\n\targ-types: ~a, ret-type: ~a\n"
                 fname (map expr-var-type args) ret-type)
       (sham:def:function
        (mod-fun-info) fname
        (map expr-var-sym args) (map (compose get&add-type expr-var-type) args)
        (get&add-type ret-type)
        (es b))]))

  (define prog (expand-fun (cons 'prog (expr-mod-main mod))))

  (sham:module
   (build-info (void) ;;TODO move this info to defs somewhere
               '((ffi-libs . ((libgslcblas . ("libgslcblas" #:global? #t))
                              (libgsl . ("libgsl"))))))
   (append (cleanup-defs (get-defs-prelude prl))
           (cons prog (map expand-fun (expr-mod-fns mod))))))
