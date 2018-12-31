#lang racket
(require "ast.rkt"
         "utils.rkt")
(provide interpret)
(define (ifunc f)
  (match f
    [(expr-fun name args ret-type body)
     (lambda arg-vals
       (check-type
        (i body (make-immutable-hash (map (λ (sym val) (cons (expr-var-sym sym) (check-type val (expr-var-type sym)))) args arg-vals)))
        ret-type))]))

(define (check-type v type)
  v)
(define (prob->real x) (exp x))
(define (real->prob x) (log x))
(define (nat->prob x) (real->prob (exact->inexact x)))

(define (get-init val t)
  (if (equal? t 'prob) (real->prob val) val))

(define (sum type v1 v2)
  (match type
    ['prob (real->prob (+ (prob->real v1) (prob->real v2)))]
    [else (+ v1 v2)]))
(define (prd type v1 v2)
  (match type
    ['prob (+ v1 v2)]
    [else (* v1 v2)]))
(define (env-extend env sym val)
  (hash-set env sym val))
(define (env-extend* env asscs)
  (for/fold ([e env])
            ([a asscs])
    (env-extend e (car a) (cdr a))))
(define env-lookup hash-ref)
(define (i expr env)
  (printf "interpreting: ~a\n" (pe expr))
  (match expr
    [(expr-lets types vars vals (stmt-void) body)
     (i body (env-extend* env (map (λ (var vale)
                                     (cons (expr-var-sym var)
                                           (check-type (i vale env) (expr-var-type var))))
                                   vars vals)))]
    [(expr-sum type ie nstart nend  body)
     (for/fold ([fval (get-init 0 type)])
               ([ind (range (check-type (i nstart env) 'nat)
                            (check-type (i nend env) 'nat))])
       (sum type fval (i body (env-extend env (expr-var-sym ie) ind))))]
    [(expr-prd type ie nstart nend  body)
     (for/fold ([fval (get-init 1 type)])
               ([ind (range (check-type (i nstart env) 'nat)
                            (check-type (i nend env) 'nat))])
       (prd type fval (i body (env-extend env (expr-var-sym ie) ind))))]
    [(expr-bucket type start end reducer)
     (define initial-val (initial-bucket reducer type))
     (do-bucket initial-val (check-type (i start env) 'nat) (check-type (i end env) 'nat) reducer)]
    [(expr-var type sym _)
     (env-lookup env sym)]
    [(expr-val t v)
     v]
    [(expr-app type rator rands)
     (apply (get-rator rator type) (map (curryr i env) rands))]))
(define (get-rator rator type)
  (match (expr-intrf-sym rator)
    ['+ +]
    ['size  vector-length]
    ['index vector-ref]))

(define (initial-bucket reducer t)
  (match* (reducer t)
    [((reducer-add _) t)
     (get-init 0 t)]
    [((reducer-split _ ra rb) `(pair ,ta ,tb))
     (mcons (initial-bucket ra ta) (initial-bucket rb tb))]
    [((reducer-fanout ra rb) `(pair ,ta ,tb))
     (mcons (initial-bucket ra ta) (initial-bucket rb tb))]
    [((reducer-index n _ ra) `(array ,tar))
     (error "todo index bucket init")
     (build-vector n (λ (i) (initial-bucket ra tar)))]
    [((reducer-nop) 'unit) 0]))

(define (do-bucket val start end reducer)
  (error "todo do-bucket"))

(define ((interpret args) st)
  (define func
    (match st
      [(state defs info os)
       (unless (equal? (length defs) 1)
         (error "more than one function for interpret"))
       (ifunc (car defs))]))
  (apply func args))
