#lang racket

(require sham/ast
         sham/jit)

(require "../ast.rkt"
         "prelude.rkt"
         "utils.rkt"
         "../utils.rkt")

(provide to-sham-lc
         debug-to-sham)
(define debug-to-sham (make-parameter #t))
(define dts (debug-printf debug-to-sham))


(define (clean-basic-type t)
  (match t
    [`(nat ,_) 'nat]
    [`(real ,_) 'real]
    [`(prob ,_) 'prob]
    [`(int ,_) 'int]
    [else t]))


(define (to-sham-lc st)
  (define prl (new-prelude))
  (add-basic&probability-defs prl)

  (define (get&add-type type-ast)
    (dts "get&add-type: ~a\n" type-ast)
    (define-values (ref defs) (get-defs&ref-type type-ast))
    (add-defs-prelude! prl defs)
    ref)

  (define (ea tresult rator rands)
    (define trands (map typeof rands))
    (define-values (rtr def)
      (get-rator (expr-intrf-sym rator) (clean-basic-type tresult) (map clean-basic-type trands)))
    (unless (void? def)
      (add-defs-prelude! prl (list def)))
    (sham:expr:app rtr (map ee rands)))

  (define (ee expr)
    (match expr
      [(expr-app t rator rands) (ea t rator rands)]
      [(expr-var t sym _) (sham:expr:var sym)]
      [(expr-val t v) (get-value v t)]
      [(expr-lets types vars vals stmt body)
       (unless (empty? vars) (dts "expr-lets: vars: ~a, types: ~a\n" (map expr-var-sym vars) (map typeof vars)))
       (define sham-types (map (compose get&add-type typeof) vars))
       (sham:expr:let (map expr-var-sym vars)
                      sham-types
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
        (list
         ;; (sham:stmt:expr
         ;;           (sham:expr:app (sham:rator:racket
         ;;                           (gensym^ 'for)
         ;;                           (λ (i) (printf "for-index var: ~a, i ~a\n" (expr-var-sym index) i))
         ;;                           (sham:type:function (list (sham:type:ref 'nat))
         ;;                                               (sham:type:ref 'void)))
         ;;                          (list (sham:expr:var (expr-var-sym index)))))
         (es body)
         (es (stmt-assign index (expr-app (typeof index) (expr-intrf '+)
                                          (list index (expr-val (typeof index) 1))))))))
      (sham:expr:void))))
  (define testput (sham:stmt:expr
                   (sham:expr:app (sham:rator:external 'libc 'putchar (sham:type:ref 'i32))
                                  (list (sham:expr:ui-value 0 (sham:type:ref 'i32))))))
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
      [(stmt-return v)
       (dtprintf "stmt-return: ~a, type: ~a\n" (pe v) (typeof v))
       (sham:stmt:return (ee v))]
      [(stmt-void) (sham:stmt:void)]
      [else (error (format "not expanding stmt: ~a\n" (ps stmt)))]))

  (define (mod-fun-info) (void)) ;;TODO
  (define (mod-info) (void))

  (define (get-debug-print-expr str)
    (sham:expr:app (sham:rator:racket (gensym^ 'debug) (λ () (printf str))
                                      (sham:type:function '() (sham:type:ref 'void)))
                   '()))
  (define (expand-fun fp)
    (match fp
      [(expr-fun fname args ret-type b)
       (define nargs (map (λ (a) (if (expr-val? a) (expr-var (expr-val-type a) '_ '()) a)) args))
       (dts "expanding-function: ~a ~a -> ~a\n"
            fname; (map expr-var-type nargs) ret-type
            (map print-sham-type (map (compose get&add-type expr-var-type) nargs))
            (print-sham-type (get&add-type ret-type)))

       (define dprf (symbol-append 'debug-print- fname))
       (sham:def:function
        (prog-fun-info  (map typeof nargs) ret-type fname)
        fname
        (map expr-var-sym nargs) (map (compose get&add-type expr-var-type) nargs)
        (get&add-type ret-type)
        (es b))]))


  (match st
    [(state prgs info passes)
     (define new-prgs (flatten (for/list ([prg prgs]) (expand-fun prg))))
     ;(printf "defs: \n~a" (pretty-format (map print-sham-def new-prgs)))
     ;; (printf "prelude: \n~a" (pretty-format (map print-sham-def (flatten (get-defs-prelude prl))))
     (run-next (append (cleanup-defs (flatten (get-defs-prelude prl))) new-prgs)
               info st)]))


  ;; (sham:module
  ;;  (basic-mod-info)
  ;;  (append (cleanup-defs (get-defs-prelude prl))
  ;;          (cons prog (map expand-fun (expr-mod-fns mod)))))
