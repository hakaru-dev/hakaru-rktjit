#lang racket

(require "utils.rkt"
         "ast.rkt"
         "simplifications.rkt")

(provide combine-loops
         debug-combine-loops)

(define debug-combine-loops (make-parameter #f))
(define dpc (debug-printf debug-combine-loops))

(define (expr-sym-append var sym t)
  (match-define (expr-var _ s o) var)
  (expr-var t (symbol-append s sym) o))

;;disabling removing cons as a part of bucket
; until we can fix the later simplifications to do the same
(define (get-init binds result t reducer)
  (dpc "get-init: result: ~a, t: ~a\n" (pe result) t)
  (match* (reducer t)
    [((reducer-add _) t)
     (values (list t) (list result) (list (expr-val t 0)) '())]

    [( (reducer-split _ ra rb) `(pair ,ta ,tb))
     #:when (expr-var? result)
     (define-values (tra vra vla ca)
       (get-init binds (expr-sym-append result 'a ta) ta ra))
     (define-values (trb vrb vlb cb)
       (get-init binds (expr-sym-append result 'b tb) tb rb))
     (values (append tra trb) (append vra vrb) (append vla vlb) (append ca cb))]
    [((reducer-split _ ra rb) `(pair ,ta ,tb))
     (define-values (tra vra vla ca)
       (get-init binds result ta ra))
     (define-values (trb vrb vlb cb)
       (get-init binds result tb rb))
     (values (list t)
             (list result)
             (list (expr-app t (expr-intrf 'cons) (list (car vla) (car vlb))))
             (cons (stmt-expr (stmt-void) (expr-app t (expr-intrf 'free-pair) (list result)))
                   (append ca cb)))]

    [( (reducer-fanout ra rb) `(pair ,ta ,tb))
     #:when (expr-var? result)
     (define-values (tra vra vla ca)
       (get-init binds (expr-sym-append result 'a ta) ta ra))
     (define-values (trb vrb vlb cb)
       (get-init binds (expr-sym-append result 'b tb) tb rb))
     (values (append tra trb) (append vra vrb) (append vla vlb) (append ca cb))]
    [((reducer-fanout ra rb) `(pair ,ta ,tb))
     (define-values (tra vra vla ca)
       (get-init binds result ta ra));;result should be car and cdr for next
     (define-values (trb vrb vlb cb)
       (get-init binds result tb rb))
     (values (list t)
             (list result)
             (list (expr-app t (expr-intrf 'cons) (list (car vla) (car vlb))))
             (cons (stmt-expr (stmt-void) (expr-app t (expr-intrf 'free-pair) (list result)))
                   (append ca cb)))]

    [((reducer-index n _ ra) `(array ,tar))
     (define fori (expr-var 'nat (gensym^ 'fi) '()))
     (define arrn (expr-var t (gensym^ 'arri) '()))
     (define new-result (expr-app tar (expr-intrf 'index) (list arrn fori)))
     (define-values (vrt vra vla vlc) (get-init (cons fori binds) new-result tar ra))
     (define arr-size (late-simplify (assign-binds binds n)))
     (define is-constant-size? (and (expr-val? arr-size) (equal? (expr-val-type arr-size) 'nat)))
     (define narrt (append `(array ,(car vrt))
                           (if is-constant-size? `((size . ,(expr-val-v arr-size))) '())))

     (when (expr-var? result) (set-expr-var-type! result narrt))
     (define-values (val cleanup)
       (if (or (equal? (car vrt) 'real) (equal? (car vrt) 'nat) (constant-size-array? (car vrt)))
           (values (expr-app narrt (expr-intrf 'empty) (list arr-size))
                   (stmt-expr (stmt-void)
                              (expr-app 'void (expr-intrf 'free) (list result))))
           (values (wrap-expr
                    narrt arrn (expr-app narrt (expr-intrf 'empty) (list arr-size))
                    (stmt-for fori (expr-val 'nat 0) arr-size
                              (stmt-assign new-result (car vla)))
                    arrn)
                   (stmt-expr
                    (stmt-for fori  (expr-val 'nat 0) arr-size (stmt-block vlc))
                    (expr-app 'void (expr-intrf 'free) (list result))))))
     (values (list narrt)
             (list result)
             (list val)
             (list cleanup))]

    [((reducer-nop) 'unit) (values '(unit) (list result) (list (expr-val 'unit 0)) '())]
    [(_ _) (error (format "get-init for bucket: t: ~a, result: ~a, reducer: ~a\n"
                          t (pe result) (pr reducer)))]))

(define (get-accum i binds result t reducer)
  ;; (printf "get-accum: result: ~a, t: ~a\n" (pe result) t)
  ;; (printf "\t binds: \n")(pretty-display (map pe binds))(newline)
  ;; (printf "\t reducer: \n")(pretty-display (pr reducer))(newline)

  (match* (reducer t)

    [((reducer-split e a b) `(pair ,ta ,tb))
     #:when (expr-var? result)
     (stmt-if (assign-binds binds e)
              (get-accum i binds (var-sym-append result ta 'a 'm) ta a)
              (get-accum i binds (var-sym-append result tb 'b 'm) tb b))]
    [((reducer-split e a b) `(pair ,ta ,tb))
     (dpc "reducer-split,accum result: ~a" (print-expr result))
     (stmt-if (assign-binds binds e)
              (get-accum i binds (expr-app ta (expr-intrf 'car) (list result)) ta a)
              (get-accum i binds (expr-app tb (expr-intrf 'cdr) (list result)) tb b))]

    [((reducer-fanout a b) `(pair ,ta ,tb))
     #:when (expr-var? result)
     (stmt-block
      (list
       (get-accum i binds (var-sym-append result ta 'a 'm) ta a)
       (get-accum i binds (var-sym-append result tb 'b 'm) tb b)))]
    [((reducer-fanout a b) `(pair ,ta ,tb))
     (dpc "reducer-split,accum result: ~a" (print-expr result))
     (stmt-block
      (list
       (get-accum i binds (expr-app ta (expr-intrf 'car) (list result)) ta a)
       (get-accum i binds (expr-app tb (expr-intrf 'cdr) (list result)) tb b)))]

    [((reducer-add e) te)
     (stmt-assign result
                  (expr-app (typeof result)
                            (expr-intrf '+)
                            (list result (assign-binds binds e))))]
    [((reducer-nop) 'unit)
     (stmt-void)]
    [((reducer-index n ind a) `(array ,tar))
     (define ind-var (expr-var 'nat (gensym^ 'indi) '_))
     (define ind-result (assign-binds binds ind))
     (stmt-expr
      (stmt-void)
      (wrap-expr 'nat ind-var ind-result
                 (get-accum i (append binds (list ind-var))
                            (expr-app (cadr (typeof result))
                                      (expr-intrf 'index) (list result ind-var))
                            tar a)
                 (expr-val 'nat 0)))]
    [(r t) (error "unknown reducer type: " r t)]))

(define (assign-binds vars bind)
  (if (and (empty? vars) (not (expr-bind? bind)))
      bind
      (match bind
        [(expr-bind v b)
         (define body (assign-binds (cdr vars) b))
         (wrap-expr (typeof body) v (car vars) (stmt-void) body)])))

;;returns (values start-val end-val) if loop else (values 0 0)
(define (loop-ends e) ;; assuming every loop starts at 0
  (values (get-start e) (get-end e)))

(define (get-start e)
  (match e
    [(expr-arr _ _ e _)    (expr-val 'nat 0)]
    [(expr-sum _ _ s e _)  s]
    [(expr-prd _ _ s e _)  s]
    [(expr-bucket _ s e _) s]))

(define (get-end e)
  (match e
    [(expr-arr _ _ e _)    e]
    [(expr-sum _ _ s e _)  e]
    [(expr-prd _ _ s e _)  e]
    [(expr-bucket _ s e _) e]))


;; var-map is all loops and this groups them by start and end being same
(define (group-same-size var-map)
  (group-by
   (λ (vv)
     (define-values (start end) (loop-ends (second vv)))
     (if (and (expr-val? start)
                (eq? (expr-val-v start) 0)
                (eq? (expr-val-type start) 'nat))
           (match end
             [(expr-app _ (expr-intrf 'size) (list (expr-var _ sym _))) sym]
             [else (print-expr end)])
           (print-expr (expr-app (expr-intrf '-) (list end start)))))
   var-map))


(define (wrap-body-for-groups loop-groups body)
  (for/fold ([b body]) ([group loop-groups])
    (define index (expr-var 'nat (gensym^ 'ci) 'm))
    ;; once we have grouped loops they all have same loop ends

    (define-values (start end) (loop-ends (second (first group))))

    (define-values (ntypes nvars nvals nstmts clstmts)
      (for/fold ([ntypes '()] [nvars '()] [nvals '()] [stmts '()] [cleanup '()]) ([loop group])

        (define var (set-mutable-var (first loop)))
        (define body (second loop))

        (define (gsf b i assigner) ;get-stmt-fold
          (set-constant-var i)
          (stmt-expr (stmt-void)
                     (wrap-expr 'nat  i index (expr->stmt b assigner) (expr-val 'nat 0))))
        (define (get-stmt-sp b i t op) ;sum and prd
          (gsf b i (λ (e) (stmt-assign var (expr-app t (expr-intrf op) (list var e))))))
        (define (get-stmt-ar b i t) ;arr
          (gsf b i (curry stmt-assign (expr-app t (expr-intrf 'index) (list var index)))))

        (match body
          [(expr-bucket t start end reducer)
           ;; (dpc "bucket: ~a : ~a\n" (pe var) t)
           (define-values (nt v l c) (get-init '() var t reducer))
           ;; (dpc "nt ~a v ~a l ~a\n" nt v (map pe l))
           (values (append nt ntypes)
                   (append (map set-mutable-var v) nvars)
                   (append l nvals)
                   (cons (get-accum index (list index) var t reducer) stmts)
                   (append c cleanup))]

          [(expr-sum t i s e b)
           (values (cons t ntypes)
                   (cons var nvars)
                   (cons (expr-val t 0) nvals)
                   (cons (get-stmt-sp b i t '+) stmts)
                   cleanup)]

          [(expr-prd t i s e b)
           (values (cons t ntypes)
                   (cons var nvars)
                   (cons (expr-val t 1) nvals)
                   (cons (get-stmt-sp b i t '*) stmts)
                   cleanup)]

          [(expr-arr t i e b)
           (define nt (if (and (expr-val? e) (equal? (expr-val-type e) 'nat))
                          (add-array-size-info t (expr-val-v e))
                          t))
           (unless (equal? t nt) (dpc "changing varinfo for var: ~a\n" (print-expr var)))
           (set-expr-var-type! var nt)

           (values (cons nt ntypes)
                   (cons var nvars)
                   (cons (expr-app nt (expr-intrf 'empty) (list e)) nvals)
                   (cons (get-stmt-ar b i t) stmts)
                   (cons (stmt-expr (stmt-void) (expr-app 'void (expr-intrf 'free) (list var))) cleanup))])))

    (define for-stmt (stmt-for index start end (stmt-block nstmts)))
    (define vl (expr-var (typeof b) (gensym^ 'vl) '()))
    (wrap-expr ntypes nvars nvals for-stmt (expr-lets (list (typeof b)) (list vl) (list b)
                                                      (stmt-block clstmts) vl))))

(define (is-loop? expr)
  (or (expr-bucket? expr) (expr-sum? expr) (expr-prd? expr) (expr-arr? expr)))

(define (wrap-body-for-normals nvm body)
  (define-values (types vars vals)
    (values (map third nvm) (map first nvm) (map second nvm)))
  ;; (dpc "wrapping-normals: ~a\n"
  ;;      (map list (map pe vars) (map pe vals) types (map typeof vars) (map typeof vals)))
  (wrap-expr types vars vals (stmt-void) body))

(define (combine-loops st)
  (define (combine-lets l)
    (match l
      [(expr-lets types vars vals (stmt-void) body)
       (define-values (loop-var-map normal-var-map)
         (partition (λ (p) (is-loop? (second p))) (map list vars vals types)))
       (define loop-groups (group-same-size loop-var-map))
       (dprint normal-var-map loop-var-map loop-groups)
       (wrap-body-for-normals normal-var-map (wrap-body-for-groups loop-groups body))]))
  (define pass
    (create-rpass
     (expr
      [(expr-lets types vars vals (stmt-void) body)
       (combine-lets (expr-lets types vars vals (stmt-void) body))])
     (reducer) (stmt) (pat)))
  (match st
    [(state prgs info os)
     (define nprgs (map pass prgs))
     ;; (dpc "combine loops:\n~a\n" (map (compose  pretty-format pe) nprgs))
     (run-next nprgs info st)]))

(define (dprint normal-var-map loop-var-map loop-groups)
  (when (not (empty? normal-var-map))
    (dpc "normal-var-map: ~a\n" (map (compose print-expr car) normal-var-map)))
  (when (not (empty? loop-var-map))
    (dpc "loop-var: ~a " (map (compose print-expr car) loop-var-map)))
  (when (not (empty? loop-groups))
    (dpc "groups: ~a\n" (map (curry map (compose print-expr car)) loop-groups))))
