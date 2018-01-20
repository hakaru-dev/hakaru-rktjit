#lang racket

(require "ast.rkt"
         "utils.rkt")

(provide compile-opts)

(define (get-value arg)
  (expr-val `(pointer ,(typeof arg))
            (or (assocv 'value (expr-var-info arg)) 0)))

(define (get-constant arg)
  (define val (get-value arg))
  (match-define (expr-var atype asym ainfo) arg)
  (define var (expr-var atype (gensym^ (symbol-append 'constant- asym)) ainfo))
  (expr-cvar var val))

(define (can-be-constant-let? triple)
  ;; todo do recursion after first step
  (match-define (list type var val) triple)
  (and (array-type? type)
       (constant-size-array? type)
       (expr-app? val)
       (equal? 'empty (expr-intrf-sym (expr-app-rator val)))
       (or (not (array-type? (array-element-type type)))
           (constant-size-array? (array-element-type type)))))
(define (get-constant-lets triple)
  (match-define (list type var val) triple)
  (define cvar (expr-var type (gensym^ (symbol-append 'constant- (expr-var-sym var))) (expr-var-info var)))
  (define cval (expr-val  `(pointer ,type) 0))
  (expr-cvar cvar val))

(define (compile-opts st)
  (define constants (list-box))
  (define pass
    (create-rpass
     (expr [(expr-lets types vars vals sbody ebody)
            (define tvv (map list types vars vals))
            (define-values (can cant) (partition can-be-constant-let? tvv))
            ;; (printf "let cans: ~a\n" (map (compose pe second) can))
            (define gls (map get-constant-lets can))
            (set-box! constants (append (unbox constants) gls))
            (expr-lets (append (map first cant) (map first can))
                       (append (map second cant) (map second can))
                       (append (map third cant) gls)
                       sbody ebody)])
     (reducer) (stmt) (pat)))
  (define (optimize fun info)
    (match fun
      [(expr-fun name args ret-type body)
       (define-values (can cant)
         (partition (λ (a)
                      (member 'constant (get-info-attrs (get-arg-info (expr-var-info a)))))
                    args))
       (printf "arg cans:~a\n" (map pe can))
       (define gl (map get-constant can))
       (set-box! constants (append (unbox constants) gl))
       (expr-fun name cant ret-type
                  (stmt-expr
                   (stmt-void)
                   (expr-lets (map typeof can) can gl
                              (pass body) (expr-val 'nat 0))))]))
  (match st
    [(state prgs info os)
     (define funs (map (curryr optimize info) prgs))

     (run-next (append (unbox constants) funs) info st)]))
