#lang racket

(require "ast.rkt")
(require "utils.rkt")
(require racket/trace)
(provide flatten-anf
         pull-indexes
         debug-flatten-anf
         debug-pull-indexes)

(define debug-flatten-anf (make-parameter #f))
(define dpf (debug-printf debug-flatten-anf))
(define debug-pull-indexes (make-parameter #f))
(define dpi (debug-printf debug-pull-indexes))
(define hakrit-loop-hoist? (make-parameter #t))

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
(define (pretty-efv e)
  (cons (print-expr (efv-var e)) (list (map print-expr (set->list (efv-fvars e))))))
(define pp pretty-print)

;; ufb: pair of expression and a list of required let bindings
;;  expr: expression,
;;  efvp: list of bindings needed by expr
(define-struct ufb (expr efvp) #:prefab)
(define (pretty-ufb u)
  (cons (print-expr (ufb-expr u)) (pretty-efvp (ufb-efvp u))))
(define (pretty-efvp efvp)
  (map pretty-efv efvp))
;;forces to form a let for `efv`'s having `var` as a free variable
(define (get-ufb-without uf vars)
  (define vset (list->set vars))
  (define sefvp (sort-efvp (ufb-efvp uf)))
  (define-values (bind free)
    (partition (λ (ef) (set-empty? (set-subtract vset (efv-fvars ef)))) sefvp))
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

;; partitions loops into groups of same size while maintaining
;;  their dependencies on each other.
;; doesn't produce fully optimal groups though
(define (partition-dependency dep-list)
  (define global-vars
    (set-subtract (apply set-union (map efv-fvars dep-list))
                  (list->set (map efv-var dep-list))))
  (define dep-hash (for/hash ([dl dep-list]) (values dl (set-subtract (efv-fvars dl) global-vars))))
  (define size-hash (for/hash ([dl dep-list]) (values dl (expr-weight (efv-expr dl)))))

  (define (rec left deps out-part)
    (define curr
      (for/hash ([g (group-by (λ (c) (hash-ref size-hash c))
                              (for/list ([(k v) (in-hash deps)]
                                         #:when (set-empty? v))
                                k))])
        (values (hash-ref size-hash (first g)) (list->set g))))
    (define curr-set (list->set
                      (set-map (apply set-union (cons (set) (hash-values curr)))
                               efv-var)))
    (define new-left (filter (λ (k) (not (set-member? curr-set (efv-var k)))) left))
    (define new-deps
      (for/hash ([c new-left])
        (values c (set-subtract (hash-ref deps c) curr-set))))
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
                       (stmt-void)
                       e))))))

(define (combine-ufb u)
  (combine-expr (ufb-expr u) (ufb-efvp u)))

(define (new-var t sym)
  (expr-var t sym sym))

(define (flatten-anf st)
  (define ffv find-free-variables)
  ;; (define (ffv expr)
  ;;   (set-subtract (find-free-variables expr) (list->set args)))
  (define (check-and-add expr efvp)
    (if (is-complex? expr)
        (let ([eufb (uf expr)])
          (values (ufb-expr eufb)
                  (append (ufb-efvp eufb) efvp)))
        (values expr efvp)))
  ;expr -> ufb
  (define (uf body)
    (match body
      [(expr-fun name args ret-type body)
       (ufb (expr-fun name args ret-type (combine-ufb (uf body))) '())]

      [(expr-lets type vars vals (stmt-void) b)
       ;;all of the lets at this point should only have void stmts
       (define nb (get-ufb-without (uf b) vars))
       ;; we hoist all the complex values, like loops and leave the rest alone
       (define-values (nvals nefvp)
         (for/fold ([nvals '()]
                    [efv (ufb-efvp nb)])
                   ([v (reverse vals)])
           (define-values (nv nefv) (check-and-add v efv))
           (values (cons nv nvals) (append nefv efv))))
       (ufb (expr-lets type vars nvals (stmt-void) (ufb-expr nb)) nefvp)]

      [(expr-sum t i start end b)
       (define es (new-var t (gensym^ 'sm)))
       (define nb (get-ufb-without (uf b) (list i)))
       (define-values (nend nefvp) (check-and-add end (ufb-efvp nb)))
       (define ns (expr-sum t i start nend (ufb-expr nb)))
       (define nefv (cons (efv es ns (ffv ns)) nefvp))
       (ufb es nefv)]

      [(expr-prd t i start end b)
       (define es (new-var t (gensym^ 'pr)))
       (define nb (get-ufb-without (uf b) (list i)))
       (define-values (nend nefvp) (check-and-add end (ufb-efvp nb)))
       (define ns (expr-prd t i start nend (ufb-expr nb)))
       (define nefv (cons (efv es ns (ffv ns)) nefvp))
       (ufb es nefv)]

      [(expr-arr t i end b)
       (define es (new-var t (gensym^ 'ar)))
       (define ub (uf b))
       (define nb (get-ufb-without ub (list i)))
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
      [else (ufb body '())]))

  (match st
    [(state prg info os)
     (define nprg (combine-ufb (uf prg)))
     (dpf "flatten-anf:\n~a\n" (pretty-format (pe nprg)))
     (run-next nprg info st)]))

;; **************************************************************** ;;
(define (pull-indexes st)
  (define combine-imaps append)
  (define (group-alpha-eq imps)
    ;;we know they all are index app so alpha eq is symbol eq
    (group-by (λ (im) (symbol-append (pe (first (expr-app-rands (cdr im))))
                                     (pe (second (expr-app-rands (cdr im))))))
              imps))
  (define (print-imp imp)
    (printf "imp: ~a-~a\n" (print-expr (car imp)) (print-expr (cdr imp))))
  (define (clean imps)
    (define aleq (group-alpha-eq imps))
    (define uniq (map first aleq))
    (append uniq
            (apply append
                   (for/list ([u uniq]
                              [rs (map cdr aleq)])
                     (for/list ([r rs])
                       (cons (car r) (car u)))))))
  (define (wrap s e imps)
    (define aleq (group-alpha-eq imps))
    (define uniq (map first aleq))
    (define dupl (apply append
                        (for/list ([u uniq]
                                   [rs (map cdr aleq)])
                          (for/list ([r rs])
                            (cons (car r) (car u))))))
    (define utvls (map (λ (i) (list (typeof (cdr i)) (car i) (cdr i))) uniq))
    (define dtvls (map (λ (i) (list (typeof (cdr i)) (car i) (cdr i))) dupl))
    (expr-lets (map first utvls) (map second utvls) (map third utvls)
               (stmt-void)
               (expr-lets
                (map first dtvls) (map second dtvls) (map third dtvls)
                s e)))

  (define (wrap-expr e imps)
    (wrap (stmt-void) e imps))
  (define (wrap-stmt s imps)
    (stmt-expr (stmt-void) (wrap s (expr-val 'nat 0) imps)))

  (define (cant-can es imap vars)
    (define ffv (apply set-union (cons (set) (map (compose find-free-variables cdr) imap))))
    (for/fold ([cant '()]
               [can '()])
              ([im imap])
      (define ffv (find-free-variables (cdr im)))
      (if (ormap (curry set-member? ffv) (append (map car imap) vars))
          (values (cons im cant) can)
          (values cant (cons im can)))))
  (define (check&wrap-f es-i vars f)
    (match-define (cons es imap) es-i)
    (define-values (cant can) (cant-can es imap vars))
    (cons (f es cant) can))

  (define (check&wrap-expr e-i vars)
    (check&wrap-f e-i vars wrap-expr))

  (define (check&wrap-stmt s-i vars)
    (check&wrap-f s-i vars wrap-stmt))

  (define (pull-stmt stmt) ;; -> (cons stmt index-map)
    (match stmt
      [(stmt-block stmts)
       (define (dostmt prv nxt)
         (if (empty? nxt) '()
             (let ([mvars (apply set-union (cons (set) (map find-mutated-variables prv)))]
                   [stmtl (pull-stmt (car nxt))])
               (cons (check&wrap-stmt stmtl (set->list mvars)) (dostmt (cons (car nxt) prv)
                                                           (cdr nxt))))))
       (define ds (dostmt '() stmts))
       (cons (stmt-block (map car ds)) (append-map cdr ds))]
      [(stmt-if tst thn els)
       (match-define (cons ntst tst-imap) (pull-expr tst))
       (match-define (cons nthn thn-imap) (pull-stmt thn))
       (match-define (cons nels els-imap) (pull-stmt els))
       (cons (stmt-if ntst nthn nels) (combine-imaps tst-imap thn-imap els-imap))]
      [(stmt-for i start end body)
       (match-define (cons body1 b-imap1) (pull-stmt body))
       (match-define (cons nbody body-imap) (check&wrap-stmt (cons body1 b-imap1) (list i)))
       (cons (stmt-for i start end nbody) body-imap)]
      [(stmt-assign lhs rhs)
       (match-define (cons nrhs rhs-imap) (pull-expr rhs))
       (cons (stmt-assign lhs nrhs) rhs-imap)]
      [(stmt-expr stmt expr)
       (match-define (cons nstmt stmt-imap) (pull-stmt stmt))
       (match-define (cons nexpr expr-imap) (pull-expr expr))
       (cons (stmt-expr nstmt nexpr) (combine-imaps stmt-imap expr-imap))]
      [(stmt-return e)
       (match-define (cons ne e-imap) (pull-expr e))
       (cons (stmt-return ne) e-imap)]
      [(stmt-void) (cons stmt '())]))

  (define (pull-index-tvl tvls)
    (define all-vars (map second tvls))
    (define var-pulls (map pull-expr (map last tvls)))
    (define-values (typs vars vals imap)
      (for/fold ([typs '()]
                 [vars '()]
                 [vals '()]
                 [imap '()])
                ([tvl (reverse tvls)])
        (match-define (list type var val) tvl)
        (match-define (cons nv v-imap) (check&wrap-expr (pull-expr val) all-vars))
        (values (cons type typs)
                (cons var vars)
                (cons nv vals)
                (append v-imap imap))))
    (cons (list typs vars vals) imap))

  (define (pull-expr expr) ;; -> (cons expr index-map)
    (match expr
      [(expr-fun name args ret-type body)
       (define expl (pull-expr body))
       (match-define (cons nbody body-imap) (check&wrap-expr expl args))
       (cons (expr-fun name args ret-type nbody) body-imap)]

      [(expr-lets types vars vals stmt expr)
       (match-define (cons (list ntypes nvars nvals) tvl-imap)
         (pull-index-tvl (map list types vars vals)))

       (define stmtl (pull-stmt stmt))
       (define expl (pull-expr expr))
       (match-define (cons nstmt stmt-imap) (check&wrap-stmt (pull-stmt stmt) nvars))

       (match-define (cons nbody body-imap)
         (check&wrap-expr expl (append nvars (set->list (find-free-variables nstmt)))))
       (cons (expr-lets ntypes nvars nvals nstmt nbody)
             (combine-imaps tvl-imap stmt-imap body-imap))]

      [(expr-if t tst thn els)
       (match-define (cons ntst tst-imap) (pull-expr tst))
       (match-define (cons nthn thn-imap) (pull-expr thn))
       (match-define (cons nels els-imap) (pull-expr els))
       (cons (expr-if t ntst nthn nels) (combine-imaps tst-imap thn-imap els-imap))]
      [(expr-app t (expr-intrf 'index) rds)
       (define ni (gensym^ 'indp))
       (define nivar (expr-var t ni csym))
       (define rdsl (map pull-expr rds))
       (define nrds (map car rdsl))
       (define rds-imps (append-map cdr rdsl))
       (cons nivar
             (combine-imaps (list (cons nivar
                                        (expr-app t (expr-intrf 'index) nrds)))
                            rds-imps))]
      [(expr-app t rtr rds)
       (define rdsl (map pull-expr rds))
       (define nrds (map car rdsl))
       (define rds-imps (append-map cdr rdsl))
       (cons (expr-app t rtr nrds) rds-imps)]
      [(? expr-var?) (cons expr '())]
      [(? expr-val?) (cons expr '())]))

  (match st
    [(state prgs info os)
     (define nprgs
       (map (λ (prg)
              (match-define (cons nprg im) (pull-expr prg))
              (unless (empty? im)
                (error "index map not empty at top level function"))
              nprg)
            prgs))
     (dpi "pull-indexes: ~a\n" (pretty-format (map pe nprgs)))
     (run-next nprgs info st)]))
