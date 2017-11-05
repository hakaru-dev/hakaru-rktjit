#lang racket

(require "ast.rkt")
(require "utils.rkt")
(require racket/trace)
(provide flatten-anf)

#|
 Does anf and let hoisting in single pass using sorting
  for dependency graph ordering, so might be slow but
  does the job for normal hakaru examples.

 ffv: find-free-variables : recursively finds free variables,
      no memoization so slow.

 uf: returns a ufb record which is the output of anf and remaining
     expressions with the variable name and free variables.
 sort-efvp: sorts a list of efv based on dependency order, uses the
            free-variable set and the variable bound in efv.
|#


;; efv: let binding information,
;; var -> expr,
;; with expr having fvars free-variables,
;; fvars: (seteqv <var>)
(define-struct efv (var expr fvars) #:prefab)
;; efvp ::= (list? efv)
(define (print-efv e)
  (cons (print-expr (efv-var e)) (list (map print-expr (set->list (efv-fvars e))))))


;; ufb: pair of expression and a list of required let bindings
;;  expr: expression,
;;  efvp: list of bindings needed by expr
(define-struct ufb (expr efvp) #:prefab)

(define hakrit-loop-hoist? (make-parameter #t))
;;forces to form a let for `efv`'s having `var` as a free variable
(define (get-ufb-without uf . vars)
  (define sefvp (sort-efvp (ufb-efvp uf)))
  (define-values (free bind)
    (splitf-at sefvp
               (λ (ef)
                 (not (ormap (λ (v) (set-member? (efv-fvars ef) v)) vars)))))

  (if (hakrit-loop-hoist?)
      (ufb (combine-expr (ufb-expr uf) bind) free)
      (ufb (combine-expr (ufb-expr uf) sefvp) '())))
;   ;;for no loop hoisting,
;; essentially force all lets now

(define (sort-efvp efvp)
  ;; (printf "sort before: ~a\n" (map (compose print-fvars efv-fvars) efvp))
  (define ret
    (sort efvp
          (λ (efv1 efv2)
            (or (set-empty? (set-subtract (efv-fvars efv1) (efv-fvars efv2)))
                (set-member? (efv-fvars efv2) (efv-var efv1))))))
  ;; (printf "sort after: ~a\n" (map (compose print-fvars efv-fvars) ret))
  ret)

(define (split-list s? l)
  (define (rec out l^)
    (if (empty? l^)
        out
        (if (s? (car l^) (caar out))
            (rec (cons (list (car l^)) out) (cdr l^))
            (rec (cons (cons (car l^) (car out)) (cdr out)) (cdr l^)))))
  (define rl (reverse l))
  (rec `((,(car rl))) (cdr rl)))

(define (partition-dependency dep-list)
;  (printf "dep-list: ~a\n" dep-list)
  (define global-vars (set-subtract (apply set-union (map efv-fvars dep-list))
                                    (list->set (map efv-var dep-list))))

  (define dep-hash (for/hash ([dl dep-list])
                     (values dl
                             (set-subtract (efv-fvars dl) global-vars))))
  (define size-hash (for/hash ([dl dep-list])
                      (values dl (expr-weight (efv-expr dl)))))
;  (printf "dep-hash\n")(pretty-print dep-hash)
;  (printf "size-hash\n") (pretty-print size-hash)
  (define (rec left deps out-part)
;    (printf "left: ~a, deps: ~a\n" (map print-efv left) deps)
    (define curr
      (for/hash ([g (group-by (λ (c) (hash-ref size-hash c))
                              (for/list ([(k v) (in-hash deps)]
                                         #:when (set-empty? v))
                                k))])
        (values (hash-ref size-hash (first g)) (list->set g))))
 ;   (printf "curr\n")(pretty-print curr)

    (define curr-set (list->set
                      (set-map (apply set-union (cons (set) (hash-values curr)))
                               efv-var)))
;    (printf "curr-set: " )(pretty-print (map print-expr (set->list curr-set)))
    (define new-left (filter (λ (k) (not (set-member? curr-set (efv-var k)))) left))
;    (printf "new-left\n")(pretty-display (map print-efv new-left))
    (define new-deps
      (for/hash ([c new-left])
        ;; (pretty-print (hash-ref deps c))
        ;; (pretty-print  curr-set)
        ;; (printf "after subtract")
        ;; (pretty-print (set-subtract (hash-ref deps c) curr-set))
        (values c (set-subtract (hash-ref deps c) curr-set))))
;    (printf "new-deps:")(pretty-print new-deps)
    (if (empty? left)
        out-part
        (rec new-left new-deps (append out-part (list curr)))))
  ;;TODO there could be cases where we can move some bindings
  ;; to a lower level and combine them with bindings of same size
  (rec (hash-keys dep-hash) dep-hash '()))
;(part-dep testd)

(define (expr-weight e)
  (match e
    [(expr-arr _ _ (expr-app 'nat (expr-intrf 'size) (list (expr-var _ a _))) _) a]
    [(expr-sum _ _ (expr-val 'nat 0)
               (expr-app 'nat (expr-intrf 'size) (list (expr-var _ a _))) _)
     a]
    [(expr-prd _ _ (expr-val 'nat 0)
               (expr-app 'nat (expr-intrf 'size) (list (expr-var _ a _))) _)
     a]
    [(expr-bucket _ (expr-val 'nat 0)
                  (expr-app 'nat (expr-intrf 'size) (list (expr-var _ a _))) _)
     a]
    [(expr-arr _ _ (expr-var _ a _) _) a]
    [(expr-sum _ _ (expr-val 'nat 0) (expr-var _ a _) _) a]
    [(expr-prd _ _ (expr-val 'nat 0) (expr-var _ a _) _) a]
    [(expr-bucket _ (expr-val 'nat 0) (expr-var _ a _) _) a]
    [else '?]))

;; encapsulate the expr with the list of let bindings
;; does it in sets based on their dependency

(define (combine-expr expr efvp)
  (if (empty? efvp)
      expr
      (begin
;        (printf "original order: ~a\n" (map print-efv efvp))
        (for/fold ([b expr])
                  ([pds (reverse (partition-dependency efvp))])

          (for/fold ([e b])
                    ([(k v) (in-hash pds)])
            (define efvs (set->list v))
            ;; (printf "size: ~a, efvs: ~a\n" k
            ;;         (map (compose print-expr efv-var) efvs))
            (expr-lets (map (λ (ef) (typeof (efv-expr ef))) efvs)
                       (map (λ (ef) (efv-var ef)) efvs)
                       (map (λ (ef) (efv-expr ef)) efvs)
                       e))))))

(define (combine-ufb u)
  (combine-expr (ufb-expr u) (ufb-efvp u)))

(define (new-var t sym)
  (expr-var t sym sym))

(define (flatten-anf expr)
  (define args (expr-fun-args (expr-mod-main expr)))
  (define (ffv expr)
    (set-subtract (find-free-variables expr) (list->set args)))
  (define (check-and-add expr efvp)
    (if (is-complex? expr)
        (let ([eufb (uf expr)])
          (values (ufb-expr eufb)
                  (append (ufb-efvp eufb) efvp)))
        (values expr efvp)))
  ;expr -> ufb
  (define (uf body)
    (match body
      [(expr-let type var val b)
       (define nb (get-ufb-without (uf b) var))
       (define-values (nval nefvp) (check-and-add val (ufb-efvp nb)))
       (ufb (expr-let type var nval (ufb-expr nb)) nefvp)]
      [(expr-lets type vars vals b)
       (define nb (apply (curry get-ufb-without (uf b)) vars))
       (define-values (nvals nefvp)
         (for/fold ([nvals '()]
                    [efv (ufb-efvp nb)])
                   ([v vals])
           (define-values (nv nefv) (check-and-add v efv))
           (values (cons nv nvals) (append nefv efv))))
       (ufb (expr-lets type vars nvals (ufb-expr nb)) nefvp)]
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
       (ufb (expr-app t rt (map ufb-expr rds-ufbs))
            (append* (map ufb-efvp rds-ufbs)))]
      [(expr-var t s o)
       (ufb body '())]
      [else  (ufb body '())]))
  (match (expr-mod-main expr)
    [(expr-fun args ret-type body)
     (define nb (uf body))
     (for ([ef (ufb-efvp nb)]) (void))
     (expr-mod (expr-fun args ret-type (combine-ufb (uf body))) '())]))
