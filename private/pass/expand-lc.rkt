#lang racket

(require sham/ast)

(require "../ast.rkt"
         "prelude.rkt"
         "../utils.rkt")

(provide expand-to-lc)

(define (expand-to-lc mod)
  (define prl (new-prelude))
  (add-basic&probability-defs prl)

  (define (get&add-type type-ast)
    (define defs (get-all-defs-type type-ast))
    (add-defs-prelude! prl defs)
    (get-type-ref (first defs)))

  (define (ea tresult rator rands)
    (define trands (map typeof rands))
    (define-values (rator def) (get-rator (expr-intrf-sym rator) tresult trands))
    (when def
      (add-defs-prelude! (list def)))
    (sham:expr:app rator (map ee rands)))

  (define (ee expr)
    (match expr
      [(expr-app t rator rands) (ea t rator rands)]
      [(expr-var t sym _) (sham:expr:var sym)]
      [(expr-val t v) (get-value v t)]
      [(expr-let t var val body)
       (sham:expr:let (list (expr-var-sym var))
                      (list (get&add-type (typeof var)))
                      (list (ee val))
                      (sham:stmt:void)
                      (ee body))]
      [(expr-lets types vars vals body)
       (sham:expr:let (map expr-var-sym vars)
                      (map (compose get&add-type typeof) vars)
                      (map ee vals)
                      (sham:stmt:void)
                      (ee body))]
      [(expr-block t stmt body)
       (sham:expr:let '() '() '() (es stmt) (ee body))]
      [(expr-if t tst thn els) (sham:expr:void)]
      [else (error (format "not expanding for expr: ~a\n" expr))]))

  (define (for->while index start end body)
    (sham:stmt:expr
     (sham:expr:let
      (list (expr-var-sym index)) (list (get&add-type (typeof index))) (list (ee start))
      (sham:stmt:while
       (ee (expr-app 'i1 (expr-intrf '<) (list index end)))
       (sham:stmt:block
        (list(es body)
             (es (stmt-assign index (expr-app (typeof index) (expr-intrf '+)
                                              (list index (expr-val (typeof index) 1))))))))
      (sham:expr:void))))

  (define (es stmt)
    (match stmt
      [(stmt-if tst thn els) (sham:stmt:if (ee tst) (es thn) (es els))]
      [(stmt-elets vars vals stmt)
       (sham:stmt:expr
        (sham:expr:let
         (map expr-var-sym vars) (map (compose get&add-type typeof) vars) (map ee vals)
         (es stmt) (sham:expr:void)))]
      [(stmt-for i start end body) (for->while i start end body)]
      [(stmt-block stmts) (sham:stmt:block (map es stmts))]
      [(stmt-assign var val) (sham:stmt:set! (ee var) (ee val))]
      [(stmt-return v) (sham:stmt:return (ee v))]
      [(stmt-void) (sham:stmt:void)]))

  (define (mod-fun-info)
    (void))
  (define (mod-info)
    (void))
  (define (expand-fun p)
    (dprintf #t "expanding function: ~a\n" (car p))
    (match (cdr p)
      [(expr-fun args ret-type b)
       (sham:def:function
        (mod-fun-info) (car p)
        (map expr-var-sym args) (map (compose get&add-type expr-var-type) args)
        (get&add-type ret-type)
        (es b))]))
  (define prog (expand-fun (cons 'prog (expr-mod-main mod))))

  (sham:module
   (mod-info)
   (append (cleanup-defs (get-defs-prelude prl))
           (cons prog
                 (map expand-fun (expr-mod-fns mod))))))
