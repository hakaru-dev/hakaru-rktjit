#lang racket

(require "ast.rkt")
(require "utils.rkt")
(require racket/trace)
(provide flatten-anf)

(define (find-free-vars expr)
  (match expr
    [(expr-sum t i start end b)
     (define fbs (find-free-vars b))
     (set-union (find-free-vars start)
                (find-free-vars end)
                (set-remove (find-free-vars b) i))]
    [(expr-let t var val b)
     (set-union
      (find-free-vars val)
      (set-remove (find-free-vars b) var))]
    [(expr-prd t i start end b)
     (set-union (find-free-vars start)
                (find-free-vars end)
                (set-remove (find-free-vars b) i))]
    [(expr-arr t i end b)
     (set-union(find-free-vars end)
               (set-remove (find-free-vars b) i))]
    [(expr-if t tst thn els)
     (set-union (find-free-vars tst)
                (find-free-vars thn)
                (find-free-vars els))]
    [(expr-app t rator rands)
     (apply set-union (map find-free-vars rands))]
    [(expr-val t v)
     (seteqv)]
    [(expr-intr sym)
     (seteqv)]
    [(expr-var t s o)
     (seteqv expr)]))

(define-struct efv (var expr fvars) #:prefab)
;; efvp ::= (list? efv)
(define-struct ufb (expr efvp) #:prefab)

;; if the expr has a fold somewhere
(define (is-complex? expr)
  (match expr
    [(expr-sum _ _ _ _ _) #t]
    [(expr-prd _ _ _ _ _) #t]
    [(expr-arr _ _ _ _) #t]
    [(expr-let _ _ v b) (or (is-complex? v) (is-complex? b))]
    [(expr-if _ tst thn els) (or (is-complex? tst) (is-complex? thn) (is-complex? els))]
    [(expr-app _ rt rds)
     (ormap is-complex? rds)]
    [(expr-var _ _ _)
     #f]
    [(expr-val _ _)
     #f]))

(define (get-ufb-without uf var)
  (define-values (esb efvp)
    (for/fold ([b (ufb-expr uf)]
               [efvs  '()])
              ([ef (ufb-efvp uf)])
      (if (set-member? (efv-fvars ef) var)
          (values (expr-let (typeof (efv-expr ef)) (efv-var ef) (efv-expr ef) b) efvs)
          (values b (cons ef efvs)))))
  (ufb esb efvp))

(define (check-and-add expr efvp)
  (if (is-complex? expr)
      (let ([eufb (uf expr)])
        (values (ufb-expr eufb)
                (append (ufb-efvp eufb) efvp)))
      (values expr efvp)))

(define (combine-ufb u)
  (for/fold ([b (ufb-expr u)])
            ([ef (ufb-efvp u)])
    (expr-let (typeof (efv-expr ef)) (efv-var ef) (efv-expr ef) b)))

(define (new-var t sym)
  (expr-var t sym sym))

(define (uf body)
  (match body
    [(expr-let type var val b)
     (define nb (get-ufb-without (uf b) var))
     (define-values (nval nefvp) (check-and-add val (ufb-efvp nb)))
     (ufb (expr-let type var nval (ufb-expr nb)) nefvp)]
    [(expr-sum t i start end b)
     (define es (new-var t (gensym^ 'sm)))
     (define nb (get-ufb-without (uf b) i))
     (define esb (ufb-expr nb))
     ;; we can always take the complex part of end expression out
     (define-values (nend nefvp) (check-and-add end (ufb-efvp nb)))
     (define nefv (efv es (expr-sum t i start nend (ufb-expr nb)) (find-free-vars esb)))
     (ufb es (cons nefv nefvp))]
    [(expr-prd t i start end b)
     (define ps (new-var t (gensym^ 'pr)))
     (define nb (get-ufb-without (uf b) i))
     (define-values (nend nefvp) (check-and-add end (ufb-efvp nb)))
     (define esb (ufb-expr nb))
     (ufb ps (cons (efv ps (expr-prd t i start nend esb) (find-free-vars esb)) nefvp))]
    [(expr-arr t i end b)
     (define as (new-var t (gensym^ 'ar)))
     (define nb (get-ufb-without (uf b) i))
     (define-values (nend nefvp) (check-and-add end (ufb-efvp nb)))
     (define esb (ufb-expr nb))
     (ufb as (cons (efv as (expr-arr t i nend esb) (find-free-vars esb)) nefvp))]
    [(expr-if t tst thn els)
     (define tufb (uf tst))
     (ufb (expr-if t (ufb-expr tufb) (combine-ufb (uf thn)) (combine-ufb (uf els)))
          (ufb-efvp tufb))]
    [(expr-app t rt rds)
     (define rds-ufbs (map uf rds))
     (ufb (expr-app t rt (map ufb-expr rds-ufbs)) (append* (map ufb-efvp rds-ufbs)))]
    [(expr-var t s o)
     (ufb body '())]
    [else  (ufb body '())]))

(define (flatten-anf expr)
  (match (expr-mod-main expr)
    [(expr-fun args ret-type body)
     (expr-mod (expr-fun args ret-type (combine-ufb (uf body))) '())]))
