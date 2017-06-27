#lang racket

(require "ast.rkt")
(require "pass-utils.rkt")
(require "utils.rkt")
(require racket/trace)
(provide flatten-anf)

#|
 Does anf and let hoisting in single pass using sorting
  for dependency graph ordering, so might be slow but
  does the job for normal hakaru examples.

 ffv: find-free-variables : recursively finds free variables,
      no memoization so slow.

 efv: sort of information for let, var -> expr, with expr having
      fvars free-variables, fvars: (seteqv <free-variables>)
 ufb: struct used to pass around information while doing anf
      expr: output expression,
      efvp: list of efv objects which are still left to be placed.
 uf: returns a ufb record which is the output of anf and remaining
     expressions with the variable name and free variables.
 sort-efvp: sorts a list of efv based on dependency order, uses the
            free-variable set and the variable bound in efv.
|#

(define-struct efv (var expr fvars) #:prefab)
;; efvp ::= (list? efv)
(define-struct ufb (expr efvp) #:prefab)

(define (get-ufb-without uf var)
  (define sefvp (sort-efvp (ufb-efvp uf)))
  (define-values (free bind)
    (splitf-at sefvp (λ (ef) (not (set-member? (efv-fvars ef) var)))))
  ;; (printf "get-ufb-witout var: ~a\n" (expr-var-sym var))
  ;; (printf "free: ~a\nbind: ~a\n"
  ;;         (map (compose print-fvars efv-fvars) free)
  ;;         (map (compose print-fvars efv-fvars) bind))
  (ufb (combine-expr (ufb-expr uf) bind) free))



(define (print-fvars fvarset)
  (define l (set->list fvarset))
  (map expr-var-sym l))

(define (sort-efvp efvp)
  ;; (printf "sort before: ~a\n" (map (compose print-fvars efv-fvars) efvp))
  (define ret
    (sort efvp
          (λ (efv1 efv2)
            (or (set-empty? (set-subtract (efv-fvars efv1) (efv-fvars efv2)))
                (set-member? (efv-fvars efv2) (efv-var efv1))))))
  ;; (printf "sort after: ~a\n" (map (compose print-fvars efv-fvars) ret))
  ret)

(define (combine-expr expr efvp)
  (for/fold ([b expr])
            ([ef (reverse efvp)])
    (expr-let (typeof (efv-expr ef)) (efv-var ef) (efv-expr ef) b)))

(define (combine-ufb u)
  (combine-expr (ufb-expr u) (sort-efvp (ufb-efvp u))))

(define (new-var t sym)
  (expr-var t sym sym))


(define (flatten-anf expr)
  (define args (expr-fun-args (expr-mod-main expr)))
  (define (ffv expr)
    (set-subtract (find-free-variables expr) (list->seteqv args)))
  (define (check-and-add expr efvp)
    (if (is-complex? expr)
        (let ([eufb (uf expr)])
          (values (ufb-expr eufb)
                  (append (ufb-efvp eufb) efvp)))
        (values expr efvp)))
  (define (uf body)
    (match body
      [(expr-let type var val b)
       (define nb (get-ufb-without (uf b) var))
       (define-values (nval nefvp) (check-and-add val (ufb-efvp nb)))
       (ufb (expr-let type var nval (ufb-expr nb)) nefvp)]
      [(expr-sum t i start end b)
       (define es (new-var t (gensym^ 'sm)))
       (define nb (get-ufb-without (uf b) i))
       (define-values (nend nefvp) (check-and-add end (ufb-efvp nb)))
       (define ns (expr-sum t i start nend (ufb-expr nb)))
       (define nefv (cons (efv es ns (ffv ns)) nefvp))
       (ufb es nefv)]
      [(expr-prd t i start end b)
       (define es (new-var t (gensym^ 'pr)))
       (define nb (get-ufb-without (uf b) i))
       (define-values (nend nefvp) (check-and-add end (ufb-efvp nb)))
       (define ns (expr-prd t i start nend (ufb-expr nb)))
       (define nefv (cons (efv es ns (ffv ns)) nefvp))
       (ufb es nefv)]
      [(expr-arr t i end b)
       (define es (new-var t (gensym^ 'ar)))
       (define nb (get-ufb-without (uf b) i))
       (define-values (nend nefvp) (check-and-add end (ufb-efvp nb)))
       (define ns (expr-arr t i nend (ufb-expr nb)))
       (define nefv (cons (efv es ns (ffv ns)) nefvp))
       (ufb es nefv)]
      [(expr-bucket t start end r)
       (define es (new-var t (gensym^ 'bk)))
       (ufb es (list (efv es body (ffv body))))]
      [x #:when (not (is-complex? x))
         (ufb x '())]
      [(expr-if t tst thn els)
       (define tufb (uf tst))
       (ufb (expr-if t (ufb-expr tufb) (combine-ufb (uf thn)) (combine-ufb (uf els)))
            (ufb-efvp tufb))]
      [(expr-match t tst brs)
       (ufb body '())]
      [(expr-app t rt rds)
       (define rds-ufbs (map uf rds))
       (ufb (expr-app t rt (map ufb-expr rds-ufbs)) (append* (map ufb-efvp rds-ufbs)))]
      [(expr-var t s o)
       (ufb body '())]
      [else  (ufb body '())]))
  (match (expr-mod-main expr)
    [(expr-fun args ret-type body)
     (define nb (uf body))
     (for ([ef (ufb-efvp nb)]) (void))
     (expr-mod (expr-fun args ret-type (combine-ufb (uf body))) '())]))
