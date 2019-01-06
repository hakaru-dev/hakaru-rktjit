#lang racket

(require sham/ast
         sham/jit
         sham/private/ast-utils
         )

(require "../ast.rkt"
         "prelude.rkt"
         "utils.rkt"
         "../utils.rkt")

(provide expand-to-sham
         debug-to-sham)
(define debug-to-sham (make-parameter #f))
(define dts (debug-printf debug-to-sham))

(define (expand-to-sham st)
  (define (ea tresult rator rands)
    (dts "ea: ~a, ~a\n" tresult (pe rator))
    (define rtr (get-sham-rator (expr-intrf-sym rator) tresult (map typeof rands)))
    (apply rtr (map ee rands))
    ;; (sham:expr:let '()'() '()
    ;;                (sham:stmt:block
    ;;                 (list (sham:stmt:void)
    ;;                       ;; (sham:stmt:expr
    ;;                       ;;  (sham:expr:app (sham:rator:racket
    ;;                       ;;                  (gensym^ 'app)
    ;;                       ;;                  (λ () (printf "app: rator: ~a, args: ~a\n" (pe rator) (map pe rands)))
    ;;                       ;;                  (sham:type:function (list )
    ;;                       ;;                                      (sham:type:ref 'void)))
    ;;                       ;;                 (list )))
    ;;                       )))
    )

  (define (ee expr)
    (dts "ee: ~a\n" (pe expr))
    (match expr
      [(expr-app t rator rands) (ea t rator rands)]
      [(expr-var t sym _) (v sym)]
      [(expr-val t v) (get-sham-value v t)]
      [(expr-cvar var #f) (evoid)]
      [(expr-cvar var val) (cllvm val (get-sham-type (typeof var)))]
      [(expr-lets types vars vals stmt body)
       (unless (empty? vars) (dts "expr-lets: vars: ~a, types: ~a\n" (map expr-var-sym vars) (map typeof vars)))
       (define sham-types (map (compose get-sham-type typeof) vars))
       (sham:ast:expr:let (map expr-var-sym vars)
                          sham-types
                          (map ee vals)
                          (block^ (es stmt)
                                  ;; (list
                                  ;;  ;; (sham:stmt:expr
                                  ;;  ;;  (sham:expr:app (sham:rator:racket
                                  ;;  ;;                  (gensym^ 'let)
                                  ;;  ;;                  (λ ()
                                  ;;  ;;                    (printf "in-let\n")
                                  ;;  ;;                    (flush-output))
                                  ;;  ;;                  (sham:type:function (list )
                                  ;;  ;;                                      (sham:type:ref 'void)))
                                  ;;  ;;                 (list )))
                                  ;;  ;; (sham:stmt:expr
                                  ;;  ;;  (sham:expr:app (sham:rator:racket
                                  ;;  ;;                  (gensym^ 'let)
                                  ;;  ;;                  (λ ()
                                  ;;  ;;                    (printf "racket let\n")
                                  ;;  ;;                    (printf "expr-lets: vars: ~a, types: ~a\n"
                                  ;;  ;;                            (map expr-var-sym vars) (map typeof vars))
                                  ;;  ;;                    (flush-output))
                                  ;;  ;;                  (sham:type:function '() (sham:type:ref 'void)))
                                  ;;  ;;                 '()))
                                  ;;  )
                                  )
                          (ee body))]
      [(expr-if t tst thn els)
       (define v (gensym^ 'if))
       (define ev (expr-var t v '_))
       (sham:ast:expr:let (list v) (list (get-sham-type t)) (list (evoid))
                          (es (stmt-if tst (stmt-assign ev thn) (stmt-assign ev els)))
                          (ee ev))]
      [else (error (format "not expanding for expr: ~a\n" (pe expr)))]))

  (define (for->while index start end body)
    (se
     (sham:ast:expr:let (list (expr-var-sym index)) (list (get-sham-type (typeof index))) (list (ee start))
                        (while^ (ee (expr-app 'bool (expr-intrf '<) (list index end)))
                                ;; (sham:stmt:expr
                                ;;           (sham:expr:app (sham:rator:racket
                                ;;                           (gensym^ 'for)
                                ;;                           (λ (i)
                                ;;                             (printf "for-index var: ~a, i ~a\n" (expr-var-sym index) i)
                                ;;                             ;; (read)
                                ;;                             )
                                ;;                           (sham:type:function (list (sham:type:ref 'nat))
                                ;;                                               (sham:type:ref 'void)))
                                ;;                          (list (sham:expr:var (expr-var-sym index)))))
                                (es body)
                                (es (stmt-assign index (expr-app (typeof index) (expr-intrf '+)
                                                                 (list index (expr-val (typeof index) 1))))))
                        (evoid))))
  (define (es stmt)
    (dts "es: ~a\n" (ps stmt))
    (match stmt
      [(stmt-if tst thn els)
       (if^ (ee tst) (es thn) (es els))]
      [(stmt-expr (stmt-void) e)
       (se (ee e))]
      [(stmt-expr s e)
       (block (list (es s) (se (ee e))))]
      [(stmt-for i start end body) (for->while i start end body)]
      [(stmt-block stmts) (block (map es stmts))]
      [(stmt-assign var val) (set!^ (ee var) (ee val))]
      [(stmt-return v)
       (dts "stmt-return: ~a, type: ~a\n" (pe v) (typeof v))
       (ret (ee v))]
      [(stmt-void) (svoid)]
      [else (error (format "not expanding stmt: ~a\n" (ps stmt)))]))

  (define (ef fp)
    (match fp
      [(expr-fun fname args ret-type b)
       (dts "expand-to-sham: \n~a\n" (pe fp))
       (define f
         (dfunction #f fname
                    (map expr-var-sym args) (map (compose get-sham-type expr-var-type) args)
                    (get-sham-type ret-type)
                    (es b)))
       (dts "final function: \n~a\n"(pretty-format f))
       f
       ;; (sham:def:function
       ;;  (prog-fun-info  (map typeof nargs) ret-type fname)
       ;;  fname
       ;;  (map expr-var-sym nargs) (map (compose get-type expr-var-type) nargs)
       ;;  (get-type ret-type)
       ;;  ;; (sham:stmt:block
       ;;  ;;  (list
       ;;  ;;   (sham:stmt:expr
       ;;  ;;    (sham:expr:app (sham:rator:racket
       ;;  ;;                    (gensym^ 'prog)
       ;;  ;;                    (λ ()
       ;;  ;;                      (printf "called-prog\n")
       ;;  ;;                      (flush-output))
       ;;  ;;                    (sham:type:function (list )
       ;;  ;;                                        (sham:type:ref 'void)))
       ;;  ;;                   (list )))
       ;;  ;;   ;; (sham:stmt:return (ee (expr-val 'nat 0)))
       ;;  ;;   (es b)))
       ;;  (es b))
       ]))


  (match st
    [(state prgs info passes)
     ;; (define new-prgs (flatten (for/list ([prg prgs]) (expand-fun prg))))
     ;(printf "defs: \n~a" (pretty-format (map print-sham-def new-prgs)))
     ;; (printf "prelude: \n~a" (pretty-format (map print-sham-def (flatten (get-defs-prelude prl))))
     (run-next (map ef prgs)
               ;; (append (cleanup-defs (flatten (get-defs-prelude prl))) new-prgs)
               info st)]))


  ;; (sham:module
  ;;  (basic-mod-info)
  ;;  (append (cleanup-defs (get-defs-prelude prl))
  ;;          (cons prog (map expand-fun (expr-mod-fns mod)))))
