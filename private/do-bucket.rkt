#lang racket

(require "utils.rkt"
         "ast.rkt"
         "to-stmt.rkt")

(provide combine-loops)

(define (expr-sym-append var sym t)
  (match-define (expr-var _ s o) var)
  (expr-var t (symbol-append s sym) o))

(define (get-init binds result t reducer)
  (printf "get-init: result: ~a, t: ~a\n" (pe result) t)
  (printf "\t binds: \n")(pretty-display (map pe binds))(newline)
  (printf "\t reducer: \n")(pretty-display (pr reducer))(newline)
  (match* (t reducer)
    [('nat (reducer-add _))
     (values (list 'nat) (list result) (list (expr-val 'nat 0)))]
    [('real (reducer-add _))
     (values (list 'real) (list result) (list (expr-val 'real 0)))]
    [(`(pair ,ta ,tb) (reducer-split _ ra rb))
     #:when (expr-var? result)
     (define-values (tra vra vla)
       (get-init binds (expr-sym-append result 'a ta) ta ra))
     (define-values (trb vrb vlb)
       (get-init binds (expr-sym-append result 'b tb) tb rb))
     (values (append tra trb) (append vra vrb) (append vla vlb))]
    [(`(pair ,ta ,tb) (reducer-split _ ra rb))
     (define-values (tra vra vla)
       (get-init binds result ta ra));;result should be car and cdr for next
     (define-values (trb vrb vlb)
       (get-init binds result tb rb))
     (values (list t)
             (list result)
             (list (expr-app t (expr-intrf 'cons) (list (car vla) (car vlb)))))]
    [(`(pair ,ta ,tb) (reducer-fanout ra rb))
     #:when (expr-var? result)
     (define-values (tra vra vla)
       (get-init binds (expr-sym-append result 'a ta) ta ra))
     (define-values (trb vrb vlb)
       (get-init binds (expr-sym-append result 'b tb) tb rb))
     (values (append tra trb) (append vra vrb) (append vla vlb))]
    [(`(array ,tar) (reducer-index n _ ra))
     (dprintf #t "reducer-index: type: ~a\n \tresult: ~a, binds: ~a\n"
              `(array ,tar) (pe result) (map pe binds))
     (define arr-size (assign-binds binds n))
     (define arr-init (expr-app tar (expr-intrf 'empty)
                                (list arr-size)))
     (define arrn (expr-var t (gensym^ 'arri) '_))
     (define fori (expr-var 'nat (gensym^ 'fi) '_))
     (define new-result (expr-app tar (expr-intrf 'index) (list arrn fori)))
     (define-values (vrt vra vla) (get-init (cons fori binds) new-result tar ra))
     (values (list t)
             (list result)
             (if (or (equal? tar 'real) (equal? tar 'nat))
                 (list arr-init)
                 (list (expr-let
                        t arrn arr-init
                        (expr-block
                         t
                         (stmt-for
                          fori (expr-val 'nat 0) arr-size
                          (stmt-assign new-result (car vla)))
                         arrn)))))]
    [('unit (reducer-nop)) (values '(unit) (list result) (list (expr-val 'unit 0)))]
    [(a b) (error (format "get-init for bucket: t: ~a, result: ~a, reducer: ~a\n"
                          t (pe result) (pr reducer)))]))

(define (get-accum i binds result t reducer)
  (printf "get-accum: result: ~a, t: ~a\n" (pe result) t)
  (printf "\t binds: \n")(pretty-display (map pe binds))(newline)
  (printf "\t reducer: \n")(pretty-display (pr reducer))(newline)

  (match* (reducer t)
    [((reducer-split e a b) `(pair ,ta ,tb))
     (dprintf #t "reducer-split,accum result: ~a" (print-expr result))
     (stmt-if (assign-binds binds e)
              (get-accum i binds
                         (if (expr-var? result)
                             (expr-var ta (symbol-append (expr-var-sym result) 'a)'_)
                             (expr-app ta (expr-intrf 'car) (list result)))
                         ta a)
              (get-accum i binds
                         (if (expr-var? result)
                             (expr-var tb (symbol-append (expr-var-sym result) 'b)'_)
                             (expr-app tb (expr-intrf 'cdr) (list result)))
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
                                   (expr-intrf '+)
                                   (list result (assign-binds binds e))))]
    [((reducer-nop) 'unit)
     (stmt-void)]
    [((reducer-index n ind a) `(array ,tar))
     (define ind-var (expr-var 'nat (gensym^ 'indi) '_))
     (define ind-result (assign-binds binds ind))
     (stmt-elets (list ind-var) (list ind-result)
                 (get-accum i (append binds (list ind-var))
                            (expr-app (cadr (typeof result))
                                      (expr-intrf 'index) (list result ind-var))
                            tar a))]
    [(r t) (printf "unknown reducer type: ~a ~a\n" r t)
           (error "reducer")]))

(define (assign-binds vars bind)
  (if (and (empty? vars) (not (expr-bind? bind)))
      bind
      (match bind
        [(expr-bind v b)
         (define body (assign-binds (cdr vars) b))
         (expr-let (typeof body) v (car vars)
                   body)])))

;;returns (values start-val end-val) if loop else (values 0 0)
(define (loop-ends e) ;; assuming every loop starts at 0
  (match e
    [(expr-arr _ _ e _) (values (expr-val 'nat 0) e)]
    [(expr-sum _ _ s e _) (values s e)]
    [(expr-prd _ _ s e _) (values s e)]
    [(expr-bucket _ s e _) (values s e)]))
;;
(define (group-same-size var-map)
  (group-by (λ (vv)
              (define-values (start end) (loop-ends (second vv)))
              (if (and (expr-val? start)
                       (eq? (expr-val-v start) 0)
                       (eq? (expr-val-type start) 'nat))
                  (match end
                    [(expr-app _ (expr-intrf 'size) (list (expr-var _ sym _))) sym]
                    [else (print-expr end)])
                  (print-expr (expr-app (expr-intrf '-) (list end start)))))
            var-map))

(define (wrap-index index body stmt)
  (if (expr-bucket? body)
      stmt ;;no need for bucket as for bucket we generate the index
      (let [(bi (match body
                  [(expr-sum t i s e b) i]
                  [(expr-prd t i s e b) i]
                  [(expr-arr t i s b)   i]))]
        (stmt-elets (list bi) (list index) stmt))))

(define (get-stmt-sp i t body ib end to-assign op)
  (wrap-index
   i body
   (expr->stmt
    ib
    (λ (e)
      (stmt-assign
       to-assign
       (expr-app t (expr-intrf op) (list to-assign end)))))))

(define (wrap-body-for-groups loop-groups body)
  (for/fold ([b body]) ([group loop-groups])
    (define index (expr-var 'nat (gensym^ 'ci) '_))
    ;; once we have grouped loops they all have same loop ends
    (define-values (start end) (loop-ends (second (first group))))
    (define-values (ntypes nvars nvals nstmts)
      (for/fold ([ntypes '()] [nvars '()] [nvals '()] [stmts '()]) ([loop group])
        (define var (first loop))
        (define body (second loop))
        (define (gsf b i assigner) ;get-stmt-fold
          (stmt-elets
           (list i) (list index)
           (expr->stmt b assigner)))

        (define (get-stmt-sp b i t op)
          (gsf b i
               (λ (e) (stmt-assign var (expr-app t (expr-intrf op)
                                                 (list var e))))))
        (define (get-stmt-ar b i t)
          (gsf b i
               (λ (e) (stmt-assign
                       (expr-app t (expr-intrf 'index) (list var index)) e))))

        (match body
          [(expr-bucket t start end reducer)
           (define-values (nt v l) (get-init '() var t reducer))
           (printf "result from get-init: vars: ~a, vals: ~a\n"
                   (map pe v) (map pe l))
           (values (append nt ntypes)
                   (append v nvars)
                   (append l nvals)
                   (cons (get-accum index (list index) var t reducer) stmts))]
          [(expr-sum t i s e b)
           (values (cons t ntypes) (cons var nvars) (cons (expr-val t 0) nvals)
                   (cons (get-stmt-sp b i t '+) stmts))]
          [(expr-prd t i s e b)
           (values (cons t ntypes) (cons var nvars) (cons (expr-val t 1) nvals)
                   (cons (get-stmt-sp b i t '*) stmts))]
          [(expr-arr t i s b)
           (values (cons t ntypes) (cons var nvars)
                   (cons (expr-app t (expr-intrf 'empty) (list s)) nvals)
                   (cons (get-stmt-ar b i t) stmts))])))
    (expr-lets ntypes nvars nvals
               (expr-block (typeof b)
                           (stmt-for index start end (stmt-block nstmts)) b))))

(define (is-loop? expr)
  (or (expr-bucket? expr)
      (expr-sum? expr)
      (expr-prd? expr)
      (expr-arr? expr)))

(define (combine-loops e)
  (define pass
    (create-rpass
     (expr

      [(expr-lets types vars vals body)
       (define-values (loop-var-map normal-var-map)
         (partition (λ (p) (is-loop? (second p))) (map list vars vals types)))
       (dprintf (not (empty? normal-var-map))
                "normal-var-map: ~a\n"
                (map (compose print-expr car) normal-var-map))
       (dprintf (not (empty? loop-var-map))
                "loop-var-map: ~a\n"
                (map (compose print-expr car) loop-var-map))

       (define loop-groups (group-same-size loop-var-map))
       (dprintf (not (empty? loop-groups))
                "loop-groups: ~a\n"
                (map (curry map (compose print-expr car)) loop-groups))
       (expr-lets
        (map third normal-var-map)
        (map first normal-var-map)
        (map second normal-var-map)
        (wrap-body-for-groups loop-groups body))])

     (reducer)
     (stmt)
     (pat)))
  (pass e))
