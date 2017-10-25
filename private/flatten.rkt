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

  (ufb (combine-expr (ufb-expr uf) bind) free))
;  (ufb (combine-expr (ufb-expr uf) sefvp) '()) ;;for no loop hoisting,
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
(define testd
  '((ar1 . ((bk5 bk7 bk4 bk8 sm6 bk6) . topic_prior))
    (bk1 . (() . w))
    (bk2 . (() . z))
    (bk3 . (() . w))
    (bk4 . (() . w))
    (bk5 . (() . w))
    (bk6 . (() . z))
    (bk7 . (() . w))
    (bk8 . (() . w))
    (pr1 . ((bk1) . topic_prior))
    (pr4 . ((bk2) . topic_prior))
    (pr6 . ((sm2 sm1) . ?))
    (pr7 . ((bk3 sm3) . topic_prior))
    (sm1 . (() . topic_prior))
    (sm2 . (() . z))
    (sm3 . (() . word_prior))
    (sm4 . (() . z))
    (sm5 . (() . topic_prior))
    (sm6 . (() . word_prior))))

(define (part-dep dep-list)
  (define dep-hash (for/hash ([dl dep-list])
                     (values (car dl) (list->set (cadr dl)))))
  (define size-hash (for/hash ([dl dep-list])
                     (values (car dl) (cddr dl))))
  (define opdep-hash (make-hash))
  (for ([(k v) (in-hash dep-hash)])
    (for ([v^ (in-set v)])
      (hash-set! opdep-hash v^ (cons k (hash-ref opdep-hash v^ '())))))
  (pretty-print dep-hash)
  (pretty-print opdep-hash)
  (pretty-print size-hash)

  (define (rec left deps out-part)
    (define curr-clear (for/list ([(k v) (in-hash deps)]
                                  #:when (empty? (car v)))
                         k))
    (define currc-size
      (for/list ([g (group-by (λ (c) (cdr (hash-ref deps c)))
                              curr-clear)])
        (cons (cdr (hash-ref deps (first g))) (list->set g))))      
    (printf "curr-clear: ~a\n" curr-clear)
    (printf "currc-size: ~a\n" currc-size)
    (define new-left (filter (λ (k) (not (member k curr-clear))) left))
    (printf "new-left~a\n" new-left)
    (define new-deps
      (for/hash ([c new-left])
        (define old (hash-ref deps c))
        (values c (cons (filter (λ (c^) (not (member c^ curr-clear)))
                                (car old))
                        (cdr old)))))
    (printf "new-dep-hash\n")(pretty-display new-deps)
    (printf "old-out-part: ~a\n" out-part)
    (define new-out-part
      (for/fold ([op out-part])
                ([cs currc-size])
        (printf "cs: ~a\n" cs)
        (append-map (λ (o) (printf "o: ~a\n" o)
                       (match o
                         [`(,a ... (,k . ,od) ,b ...)
                          #:when (equal? k (car cs))
                          (define abd (apply set-union (map cdr (append a b))))
                          (define next-set (set))
                          (for ([c (cdr cs)])
                            (if (set-empty? (set-intersect abd
                                                           (hash-ref dep-hash c)))
                                (set-add! od c)
                                (set-add! next-set c))
                            (printf "c: ~a\n" c))
                          (printf "newod: ~a, next-set: ~a" od next-set)
                          (if (set-empty? next-set)
                              (list o)
                              (list o (cons (car k) next-set)))]
                          
                          
                         [else (list o)]))
                    op)))
        
    (if (empty? new-left)
        currc-size
        (rec new-left new-deps (list currc-size))))
  (rec (hash-keys dep-hash) dep-hash '()))
  
  


(part-dep testd)

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
    [else '?]))
(define (partition-dependency efvp)
  (if (empty? efvp)
      '()
      (let ([global-vars (set-subtract (apply set-union (map efv-fvars efvp))
                                    (list->set (map efv-var efvp)))])
        (define dep-efv (for/list ([e efvp])
                          (efv (efv-var e)
                               (efv-expr e)
                               (set-subtract (efv-fvars e) global-vars))))
        (define dep-map (for/hash ([e efvp])
                          (values (print-expr (efv-var e))
                                  (cons (map print-expr (set->list (set-subtract (efv-fvars e) global-vars)))
                                        (expr-weight (efv-expr e))))))
        (printf "dep-map ~a\n" dep-map)
        (printf "dep-efv: ~a\n" (map print-efv dep-efv))
        (define sorted-efvs (sort dep-efv
                                  (λ (p1 p2) (not (set-member? (efv-fvars p1) (efv-var p2))))))
        (printf "sorted-efv: ~a\n" (map print-efv
                                       sorted-efvs))
        (define grouped-efvs (split-list
                              (λ (p1 p2)
                                (set-member? (efv-fvars p2) (efv-var p1)))
                              sorted-efvs))
        (printf "grouped-efvs: ~a\n" (map (λ (g)
                                            (map (λ (gp) (print-expr (efv-var gp))) g))
                                          grouped-efvs))
        grouped-efvs)))

;; encapsulate the expr with the list of let bindings
;; does it in sets based on their dependency

(define (combine-expr expr efvp)
  (for/fold ([b expr])
            ([ef (partition-dependency efvp)])
;    (printf "ef: ~a\n" (map (λ (e) (print-expr (efv-var e))) ef))
    (if (eq? (length ef) 1)
        (expr-let (typeof (efv-expr (car ef))) (efv-var (car ef)) (efv-expr (car ef)) b)
        (expr-lets (map (λ (e) (typeof (efv-expr e))) ef)
                   (map (λ (e) (efv-var e)) ef)
                   (map (λ (e) (efv-expr e)) ef)
                   b))))

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
       (define-values (nvals nefvp) (for/fold ([nvals '()]
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
