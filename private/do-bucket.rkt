#lang racket

(require "utils.rkt")
(require "ast.rkt")

(provide bucket->for
         combine-loops)

(define bucket->for
  (expr/pass
   [(expr-lets typs vars vals body)
    (define-values  (ntyps nvars nvals stmts)
      (for/fold ([ntyps '()]
                 [nvars '()]
                 [nvals '()]
                 [stmts '()])
                ([t typs]
                 [var vars]
                 [val vals])
        (match val
          [(expr-bucket t start end reducer)
           (define-values (typs vars vals stmt) (do-bucket var t start end reducer))
           (values (append typs ntyps) (append vars nvars) (append vals nvals)
                   (cons stmt stmts))]
          [else (values (cons t ntyps) (cons var nvars) (cons val nvals) stmts)])))
    (define nbody (expr-block (typeof body) (stmt-block stmts) body))
    (expr-lets (typeof nbody) nvars nvals nbody)]
   [(expr-let t var (expr-bucket t start end reducer) body)
    (define-values (typs vars vals stmt) (do-bucket var t start end reducer))
    (define nbody (expr-block t stmt body))
    (expr-lets (typeof nbody) vars vals nbody)]))

(define (do-bucket result t start end reducer)
    (define ind (expr-var 'nat (gensym^ (symbol-append (expr-var-sym result) 'i)) '_))
    (define-values (init-types init-vars init-vals) (get-init '() result t reducer))
    (define red-stmt (stmt-for ind start end (get-accum ind (list ind) result t reducer)))
    (values init-types init-vars init-vals red-stmt))

(define (expr-sym-append var sym t)
  (match-define (expr-var _ s o) var)
  (expr-var t (symbol-append s sym) o))

(define (get-init binds result t reducer)
  (match* (t reducer)
    [('nat (reducer-add _)) (values (list 'nat) (list result) (list (expr-val 'nat 0)))]
    [(`(pair ,ta ,tb) (reducer-split _ ra rb))
     (define-values (tra vra vla) (get-init binds (expr-sym-append result 'a ta) ta ra))
     (define-values (trb vrb vlb) (get-init binds (expr-sym-append result 'b tb) tb rb))
     (values (append tra trb ) (append vra vrb) (append vla vlb))]
    [(`(pair ,ta ,tb) (reducer-fanout ra rb))
     (define-values (tra vra vla) (get-init binds (expr-sym-append result 'a ta) ta ra))
     (define-values (trb vrb vlb) (get-init binds (expr-sym-append result 'b tb) tb rb))
     (values (append tra trb) (append vra vrb) (append vla vlb))]
    [(`(array ,tar) (reducer-index n _ ra))
     (printf "reducer-index: ~a\n" (pe result))
     (define ptar (get-print-type `(array ,tar)))
     (define arr-size (assign-binds binds n))
     (define arrn (expr-var t (gensym^ 'arri) '_))
     (define fori (expr-var 'nat (gensym^ 'fi) '_))
     (define arr-init (expr-app tar (expr-intrf (symbol-append 'empty- ptar))
                                (list arr-size)))
     (define-values (vrt vra vla) (get-init (cons fori binds) result tar ra))
     (define arrv
       (expr-let
        '? arrn arr-init
        (expr-block
         '?
         (stmt-for
          fori (expr-val 'nat 0) arr-size
          (stmt-assign
           (expr-app
            '?
            (expr-intr 'index)
            (list arrn fori))
           (car vla)))
         arrn)))
     (values (list t)
             (list result)
             (list arrv))]
    [('unit (reducer-nop)) (values '() '() '())]))

(define (get-accum i binds result t reducer)
  (match* (reducer t)
    [((reducer-split (expr-bind bvar bbody) a b) `(pair ,ta ,tb))
     (stmt-if (expr-let (typeof i) bvar i bbody)
              (get-accum i binds
                         (expr-var ta (symbol-append (expr-var-sym result) 'a) '_)
                         ta a)
              (get-accum i binds
                         (expr-var tb (symbol-append (expr-var-sym result) 'b) '_)
                         tb b))]
    [((reducer-fanout a b) `(pair ,ta ,tb))
     (stmt-block
      (list
       (get-accum i binds
                  (expr-var ta (symbol-append (expr-var-sym result) 'a) '_)
                  ta a)
       (get-accum i binds
                  (expr-var tb (symbol-append (expr-var-sym result) 'b) '_)
                  tb b)))]
    [((reducer-add e) te)
     (stmt-assign result (expr-app (typeof result)
                                   (expr-intr '+)
                                   (list result (assign-binds binds e))))]
    [((reducer-nop) 'unit)
     (stmt-void)]
    [((reducer-index n ind a) ti)
     (define ind-var (expr-var 'nat (gensym^ 'indi) '_))
     (define ind-result (assign-binds binds ind))
     (stmt-lets (list ind-var)
                (stmt-block
                 (list
                  (stmt-assign ind-var ind-result)
                  (get-accum i (append binds (list ind-var))
                             (expr-app (cadr (typeof result))
                                       (expr-intr 'index) (list result ind-var))
                             t a))))]
    [(r t) (printf "unknown reducer type: ~a\n" t)]))

(define (assign-binds vars bind)
  (if (and (empty? vars) (not (expr-bind? bind)))
      bind
      (match bind
        [(expr-bind v b)
         (define body (assign-binds (cdr vars) b))
         (expr-let (typeof body) v (car vars)
                   body)])))

(define (combine-loops e)
  (define (loop-ends e) ;; assuming every loop starts at 0
    (match e
      [(expr-arr _ _ e _) (values (expr-val 'nat 0) e)]
      [(expr-sum _ _ s e _) (values s e)]
      [(expr-prd _ _ s e _) (values s e)]
      [(expr-bucket _ s e _) (values s e)]))
  (define (group-same-size var-map)
    (group-by (λ (vv)
                (define-values (start end) (loop-ends (second vv)))
                (if (and (expr-val? start)
                         (eq? (expr-val-v start) 0)
                         (eq? (expr-val-type start) 'nat))
                    (begin
                      (match end
                       [(expr-app _ (expr-intr 'size) (list (expr-var _ sym _))) sym]
                       (print-expr end)))
                    (print-expr (expr-app (expr-intr '-) (list end start)))))
              var-map))
  (define (get-loop-stuff var val index)
    (match val
      [(expr-bucket t start end reducer)
       (define-values (nt v l) (get-init '() var t reducer))
       (values nt v l (get-accum index (list index) var t reducer))]
      [(expr-sum t i s e b)
       (values (list t) (list var) (list (expr-val t 0))
               (stmt-assign var (expr-app t (expr-intr '+) (list var b))))]
      [(expr-prd t i s e b)
       (values (list t) (list var) (list (expr-val t 1))
               (stmt-assign var (expr-app t (expr-intr '*) (list var b))))]
      [(expr-arr t i s b)
       (values
        (list t)
        (list var)
        (list (expr-app t (expr-intrf (symbol-append 'empty- (get-print-type t)))
                        (list s)))
        (stmt-assign (expr-app t (expr-intr 'index) (list var index))
                     b))]))
  (define (wrap-index index body stmt)
    (if (expr-bucket? body)
        stmt
        (let [(bi (match body
                 [(expr-sum t i s e b) i]
                 [(expr-prd t i s e b) i]
                 [(expr-arr t i s b)   i]))]
          (stmt-lets (list bi) (stmt-block (list (stmt-assign bi index) stmt))))))
  (define pass
    (create-rpass
    (expr
     [(expr-lets type vars vals body)
      (define var-map-groups (group-same-size (for/list ([vr vars] [vl vals]) (list vr vl))))
      (define new-vars (map first (apply append var-map-groups)))
      (define new-vals (map second (apply append var-map-groups)))
      (define new-b
        (for/fold ([b body]) ([vmg var-map-groups])
          (define index (expr-var 'nat (gensym^ 'ci) '_))
          (define-values (start end)
            (match (cadar vmg)
              [(expr-bucket _ start end _) (values start end)]
              [(expr-sum t i s e b) (values s e)]
              [(expr-prd t i s e b) (values s e)]
              [(expr-arr t i s b) (values (expr-val 'nat 0) s)]))
          (define-values (ntypes nvars nvals nstmts)
            (for/fold ([ntyps '()] [nvars '()] [nvals '()] [nstmt '()]) ([vm vmg])
              (define-values (nt nv nl ns) (get-loop-stuff (first vm) (second vm) index))
              (values (append nt ntyps) (append nv nvars) (append nl nvals)
                      (cons (wrap-index index (second vm) ns) nstmt))))
          (expr-lets ntypes nvars nvals
                     (expr-block type (stmt-for index start end (stmt-block nstmts)) b))))
      new-b])
    (reducer)
    (stmt)
    (pat)))
  (pass e))

