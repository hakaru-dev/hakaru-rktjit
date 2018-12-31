#lang racket

(require "ast.rkt"
         "utils.rkt")

(provide initial-simplifications
         debug-initial-simplifications
         middle-simplifications
         later-simplifications
         debug-later-simplifications
         late-simplify
         remove-pairs
         fix-loop-lets)

(define debug-initial-simplifications (make-parameter #f))
(define dpi (debug-printf debug-initial-simplifications))

(define debug-later-simplifications (make-parameter #f))
(define dpl (debug-printf debug-later-simplifications))


;; stuff we can easily remove/simplify before running flatten-anf
(define (initial-simplifications st)
  (define (toif t tst brs)
    (define-values (tb fb)
      (if (pat-true? (expr-branch-pat (car brs)))
          (values (car brs) (cadr brs))
          (values (cadr brs) (car brs))))
    (expr-if t tst (expr-branch-body tb) (expr-branch-body fb)))
  (define (extract-pair t tst brs)
    (unless (and (eq? (length brs) 1)
                 (eq? (car (typeof tst)) 'pair)
                 (pat-pair? (expr-branch-pat (car brs)))
                 (pat-var? (pat-pair-a (expr-branch-pat (car brs))))
                 (pat-var? (pat-pair-b (expr-branch-pat (car brs)))))
      (error "matching over pair with multiple branches or complex pattern." brs))
    (dpi "init-simple: extract-pair: t: ~a, tst: ~a : ~a, \n" t (pe tst) (typeof tst))
    (define-values (car-type cdr-type)
      (match (typeof tst)
        [`(pair ,ta ,tb) (values ta tb)]))
    (define-values (car-var cdr-var body)
      (match (car brs)
        [(expr-branch _ (expr-bind av (expr-bind bv b)))
         (values av bv b)]))

    (set-expr-var-type! car-var car-type)
    (set-expr-var-type! cdr-var cdr-type)

    (dpi "\t ~a\t: ~a\n" (print-expr car-var) car-type)
    (dpi "\t ~a\t: ~a\n" (print-expr cdr-var) cdr-type)

    (if (expr-var? body)
        (match body
          [(expr-var t sym o)
           #:when (equal? sym (expr-var-sym car-var))
           (expr-app car-type (expr-intrf 'car) (list tst))]
          [(expr-var t sym o)
           #:when (equal? sym (expr-var-sym cdr-var))
           (expr-app cdr-type (expr-intrf 'cdr) (list tst))])
        (let ([car-val (expr-app car-type (expr-intrf 'car) (list tst))]
              [cdr-val (expr-app cdr-type (expr-intrf 'cdr) (list tst))])
          (dpi "init-simple: types: car: ~a, cdr: ~a\n" (typeof car-val) (typeof cdr-val))
          (expr-lets
           (list car-type cdr-type)
           (list car-var cdr-var)
           (list car-val cdr-val)
           (stmt-void)
           body))))

  (define (filter-index pred? lst)
    (for/list ([i (in-range (length lst))] [v lst] #:when (pred? i)) v))

  (define (make-switch t lst v) ;;TODO actual switch maybe? :P
    (dpi "make-switch: ~a\n" (map typeof lst))
    (for/fold ([b (expr-val t 0)])
              ([e lst]
               [i (in-range (length lst))])
      (expr-if t (expr-app 'bool (expr-intrf '==) (list v (expr-val t i))) e b)))
  (define pass
    (create-rpass
     (expr [(expr-app t (expr-intrf 'empty) '())
            (expr-app t (expr-intrf 'empty) (list (expr-val 'nat 0)))]

           [(expr-app t (expr-intrf 'dirac) (list val))
            (dpi "dirac: t: ~a, tval: ~a\n" t (typeof val))
            val]

           [(expr-app t (expr-intrf 'pose) (list arg1 arg2)) arg2]

           [(expr-app t (expr-intrf 'superpose) args)
            (dpi "superpose: t: ~a\n" t)
            (define get-odds (curry filter-index odd?))
            (define get-evens (curry filter-index even?))
            (define var (expr-var t (gensym^ 's) '_))
            (wrap-expr t var
                       (expr-app t (expr-intrf 'superpose-categorical)
                                 (get-evens args))
                       (stmt-void)
                       (make-switch t (get-odds args) var))]

           [(expr-app t (expr-intrf 'mbind)
                      (list val (expr-bind (expr-var vt var org-sym) body)))
            (dprintf #t "mbind: ~a : ~a\n" var (typeof val))
            (wrap-expr t (expr-var (typeof val) var org-sym) val (stmt-void) body)]

           [(expr-app ta (expr-intrf 'index)
                      (list (expr-app t (expr-intrf 'array-literal) aargs) iarg))
            (define (check-if-remove arr-vals indexer orig-b)
              (if (complex? indexer)
                  orig-b
                  (match indexer
                    [(expr-if t chk (expr-val 'nat vthn) (expr-val 'nat vels))
                     (expr-if t chk (list-ref arr-vals vthn) (list-ref arr-vals vels))]
                    [else orig-b])))
            (define ab
              (expr-app ta (expr-intrf 'index)
                        (list (expr-app t (expr-intrf 'array-literal) aargs) iarg)))
            (if (< (length aargs) 5) (check-if-remove aargs iarg ab) ab)]
           [(expr-app ta (expr-intrf 'size) (list arg))
            #:when (constant-size-array? (typeof arg))
            (expr-val ta (get-size-of-array (typeof arg)))]

           [(expr-app ta (expr-intrf 'index) (list (expr-var tv vv info) ind))
            (if (is-constant-type? (second tv))
                (expr-val ta (get-constant-value (second tv)))
                (expr-app ta (expr-intrf 'index) (list (expr-var tv vv info) ind)))]
           [(expr-app ta (expr-intrf 'index)
                      (list (expr-app t (expr-intrf 'constant-value-array) cargs) ind))
            (second cargs)]
           [(expr-sum t i start end (expr-val 'prob v))
            (expr-app 'prob (expr-intrf 'real2prob)
                      (list (expr-app 'real (expr-intrf 'nat2real)
                                 (list
                                  (expr-app 'nat (expr-intrf '*)
                                            (list
                                             (expr-app 'nat (expr-intrf '-)
                                                       (list end start))
                                             (expr-val 'real (exp v))))))))]
           ;; TODO ^^ remove this

           [(expr-match t tst brs)
            (if (eq? (typeof tst) 'bool) (toif t tst brs) (extract-pair t tst brs))]
           [(expr-var t sym info)
            #:when (and (pair? t)
                        (member (car t) '(nat real int))
                        (assocv 'constant (cdr t)))
            (dpi "init-simple: got constant from info replacing: ~a=~a\n"
                 sym (assocv 'constant (cdr t)))
            (expr-val t (assocv 'constant (cdr t)))])
     (reducer)
     (stmt)
     (pat)))
  (match st
    [(state prgs info os)
     (define nprgs (map pass prgs))
     (dpi "initial simplification:\n~a\n" (pretty-format (map pe nprgs)))
     (run-next nprgs info st)]))


(define (middle-simplifications st)
  (define (sl ast)
    (match ast
      [(expr-fun name args ret-type body)
       (expr-fun name args ret-type (sl body))]
      [(expr-lets ts vars vals stmt body)
       (expr-lets ts vars (map sl vals) (sl stmt) (sl body))]
      [(expr-prd t i start (expr-if te tst thn els) b)
       (sl (expr-if te tst
                    (expr-prd t i start thn b)
                    (expr-prd t i start els b)))]
      [(expr-prd t i start (expr-val tv 0) _)
       (expr-val t 1)]
      [(expr-prd t i start end (expr-val tv 1))
       (expr-val t 1)]
      [(expr-prd t i start end (expr-if tb tst thn (expr-val tv 1)))
       #:when (set-member? (find-free-variables tst) i)
       (match tst
          [(expr-app _ (expr-intrf '==) (list  v1 v2))
           #:when (or (equal? v1 i) (equal? v2 i))
           (sl (expr-lets (list (typeof i))
                       (list i)
                       (list (if (equal? v1 i) v2 v1))
                       (stmt-void)
                       thn))]
          [else (expr-prd t i start end (sl (expr-if tb tst thn (expr-val tv 1))))])]
      [(expr-prd t i start end (expr-if tb tst thn els))
       (sl (expr-if tb tst
                    (expr-prd t i start end thn)
                    (expr-prd t i start end els)))]
      ;; TODO verify ^^

      [(expr-prd t i start end b)
       (define nb (sl b))
       (if (expr-if? nb)
           (sl (expr-prd t i start (sl end) nb))
           (expr-prd t i start (sl end) nb))]
      [(expr-sum t i start end b)
       (expr-sum t i start (sl end) (sl b))]
      [(expr-arr t i end b)
       (expr-arr t i (sl end) (sl b))]

      [(expr-if t tst thn els)
       (expr-if t (sl tst) (sl thn) (sl els))]
      [(expr-app t rator rands)
       (expr-app t rator (map sl rands))]

      ;; stmt simplifications
      [(stmt-for i start end b)
       (stmt-for i (sl start) (sl end) (sl b))]
      [(stmt-expr s e)
       (stmt-expr (sl s) (sl e))]
      [(stmt-block stmts)
       (stmt-block (map sl stmts))]
      [(stmt-void) (stmt-void)]
      [(stmt-if tst thn els)
       (stmt-if (sl tst) (sl thn) (sl els))]
      [(stmt-assign lhs rhs)
       (stmt-assign lhs (sl rhs))]
      [else ast]))
  (match st
    [(state prgs info os)
     (define nprgs (map sl prgs))
     (run-next nprgs info st)]))

(define (late-simplify e)
  (define (clean-expr-lets e)
    (match e
      [(expr-lets '() '() '() (stmt-void) e)
       (clean-expr-lets e)]
      [(expr-lets (list t) (list var) (list val) (stmt-void) e)
       #:when (and (expr-var? e) (equal? var e))
       (dprintf (not (equal? (typeof val) (typeof e)))
                (string-append  "was cleaning up var but got weird types:"
                                "\n\tvar ~a:~a, \n\tval ~a:~a, \n\tbody ~a:~a\n\n")
                (pe var) (typeof var) (pe val) (typeof val) (pe e) (typeof e))
       val]
      [else e]))
  (define (canbereal? e)
    (match e
      [(expr-app t (expr-intrf s) rands)
       (or (set-member? (list->set '(nat2prob real2prob)) s)
           (and (equal? s 'betafunc) (andmap canbereal? rands)))]
      [else #f]))
  (define (get-real e)
    (match e
      [(expr-app 'prob (expr-intrf 'nat2prob) rands)
       (expr-app 'real (expr-intrf 'nat2real) rands)]
      [(expr-app 'prob (expr-intrf 'real2prob) rands)
       #:when (equal? (length rands) 1)
       (first rands)]
      [(expr-app 'prob (expr-intrf 'betafunc) rands)
       (expr-app 'real (expr-intrf 'realbetafunc) (map get-real rands))]))

  (define (sl e env)
    (match e
      [(expr-fun name args ret-type body)
       (expr-fun name args ret-type (sl body env))]
      [(expr-lets '() '() '() (stmt-void) b)
       (sl b env)]
      [(expr-lets ts vars vals stmt body)

       (define bffv (set-union (find-free-variables body)
                               (find-free-variables stmt)
                               (apply set-union
                                      (cons (set)
                                            (map find-free-variables vals)))))

       (dpl "simplifying-expr-lets: ~a\n"
            (pretty-format (map list
                                (map pe vars) ts
                                (map typeof vars)
                                (map typeof vals))))

       (define-values (nts nvars nvals ne)
         (for/fold ([nts '()] [nvars '()] [nvals '()] [e env])
                   ([t ts] [var vars] [val vals])
           (define nv (sl val e))
           (cond
             [(not (set-member? bffv var))
              (dpl "removing: ~a as fvars~a\n" (pe var) (map pe (set->list bffv)))
              (values nts nvars nvals e)]
             [(and (expr-var? nv) (not (is-mutable-var? var)))
              (dpl "replacing-immutable: ~a <- ~a\n" (pe var) (pe val))
              (values nts nvars nvals (hash-set e var nv))]
             [else
              (values (cons t nts) (cons var nvars) (cons nv nvals) e)])))
       (clean-expr-lets
        (expr-lets (reverse nts) (reverse nvars) (reverse nvals)
                   (sl stmt ne) (sl body ne)))]

      [(expr-if t tst thn els)
       (expr-if t (sl tst env) (sl thn env) (sl els env))]


      ;; removing redundant logfloatconversions
      [(expr-app 'prob (expr-intrf '+) args)
       #:when (andmap canbereal? args)
       (sl (expr-app 'prob (expr-intrf 'real2prob)
                     (list (expr-app 'real (expr-intrf '+) (map get-real args))))
           env)]

      [(expr-app t1 (expr-intrf 'car) (list (expr-app `(pair ,t1 unit) (expr-intrf 'index) (list arr ind))))
       (sl (expr-app t1 (expr-intrf 'index) (list arr ind)) env)]
      [(expr-app t1 (expr-intrf 'cdr) (list (expr-app `(pair unit ,t1) (expr-intrf 'index) (list arr ind))))
       (sl (expr-app t1 (expr-intrf 'index) (list arr ind)) env)]


      [(expr-app 'prob (expr-intrf '*) args)
       #:when (andmap canbereal? args)
       (sl (expr-app 'prob (expr-intrf 'real2prob)
                     (list (expr-app 'real (expr-intrf '*) (map get-real args))))
           env)]

      [(expr-app 'prob (expr-intrf 'betafunc) args)
       #:when (andmap canbereal? args)
       (sl (expr-app 'prob (expr-intrf 'betafuncreal) (map get-real args)) env)]

      [(expr-app t (expr-intrf 'superpose-categorical) args)
       (define nargs (map (curryr sl env) args))
       (if (andmap canbereal? nargs)
           (expr-app t (expr-intrf 'superpose-categorical-real)
                     (map get-real nargs))
           (expr-app t (expr-intrf 'superpose-categorical)
                     nargs))]
      ;;end removing logfloat conversions

      [(expr-app t rator rands)
       (expr-app t rator (map (curryr sl env) rands))]

      ;; stmt simplifications
      [(stmt-for i start end b)
       (stmt-for i (sl start env) (sl end env) (sl b env))]

      [(stmt-expr s e)
       (define se (stmt-expr (sl s env) (sl e env)))
       (match se
         [(stmt-expr (stmt-void) (expr-lets '() '() '() s (expr-val 'nat 0)))
          (sl s env)]
         [(stmt-expr (stmt-void) (expr-val 'nat 0))
          (stmt-void)]
         [else se])]

      [(stmt-block '())
       (stmt-void)]
      [(stmt-block stmts)
       #:when (and (andmap stmt-expr? stmts)
                   (andmap (λ (se) (stmt-void? (stmt-expr-stmt se))) stmts)
                   (andmap (λ (se) (match (stmt-expr-expr se)
                                     [(expr-lets ts vs vls s (expr-val t 0)) #t]
                                     [else #f]))
                           stmts))
       (define tvls
         (map
          (λ (se)
            (match (stmt-expr-expr se)
              [(expr-lets ts vs vls s (expr-val t 0)) (list ts vs vls s)]))
          stmts))
       (stmt-expr (stmt-void)
                  (sl
                   (expr-lets (append-map first tvls)
                              (append-map second tvls)
                              (append-map third tvls)
                              (sl (stmt-block (map fourth tvls)) env)
                              (expr-val 'nat 0))
                   env))]
      [(stmt-block (list s)) (sl s env)]

      [(stmt-block stmts)
       (define nstmts (map (curryr sl env) stmts))
       (define (combine-ifs stmts)
         (define if-groups (group-by (λ (s) (stmt-if-tst s)) stmts))
         (for/list ([g if-groups])
           (stmt-if (stmt-if-tst (first g))
                    (sl (stmt-block (map stmt-if-thn g)) env)
                    (sl (stmt-block (map stmt-if-els g)) env))))
       (if (andmap stmt-if? nstmts)
           (stmt-block (combine-ifs nstmts))
           (stmt-block nstmts))]

      [(stmt-void) (stmt-void)]

      [(stmt-if tst thn els)
       (stmt-if (sl tst env) (sl thn env) (sl els env))]

      [(stmt-assign lhs rhs)
       #:when (expr-app? lhs)
       (sl
        (stmt-expr
         (stmt-void)
         (match lhs
           [(expr-app t (expr-intrf 'car) args)
            (expr-app 'void (expr-intrf 'set-car!) (append args (list rhs)))]
           [(expr-app t (expr-intrf 'cdr) args)
            (expr-app 'void (expr-intrf 'set-cdr!) (append args (list rhs)))]
           [(expr-app t (expr-intrf 'index) args)
            (expr-app 'void (expr-intrf 'set-index!) (append args (list rhs)))]))
        env)]
      [(stmt-return v)
       (stmt-return v)]

      [(expr-sum t i start end b)
       (expr-sum t (sl i env) (sl start env) (sl end env) (sl b env))]
      [(expr-prd t i start end b)
       (expr-prd t (sl i env) (sl start env) (sl end env) (sl b env))]
      [(expr-arr t i end b)
       (expr-arr t (sl i env) (sl end env) (sl b env))]
      [(expr-bucket t start end r)
       (expr-bucket t start end r)]

      [v #:when (hash-has-key? env v)
         (dpl "\treplaced: ~a:~a with ~a:~a\n"
              (print-expr v) (typeof v) (print-expr (hash-ref env v))
              (typeof (hash-ref env v)))
         (hash-ref env v)]
      [v #:when (or (expr-var? v) (expr-val? v)) v]

      [(stmt-assign lhs rhs)
       (stmt-assign lhs (sl rhs env))]
      [(? stmt?) (error "stmt not done: ~a\n" (ps e))]
      [(? expr?) (error "expr not done: ~a\n" (pe e))]
      [else (error 'notdone)]))
  (sl e (make-immutable-hash)))

;;stuff that come up after flatten and combining loops that we can remove or simplify
;; this has an env which stores the bindings uptil that expression or statements
;; so all the stuff which depends on environment should come here,
(define (later-simplifications st)
  (match st
    [(state prgs info os)
     (define nprgs (map late-simplify prgs))
     (run-next nprgs info st)]))

(define (remove-pairs st)
  (define (rp e env)
    (match e
      [(expr-app t (expr-intrf s) (list arg))
       #:when (member s '(car cdr))
       (define narg (rp arg env))
       (if (and (expr-var? narg)
                (ormap (λ (vs) (string-prefix? vs (symbol->string (expr-var-sym narg)))) (hash-keys env)))
           (let ([sym (symbol-append (expr-var-sym narg)
                                     (if (equal? s 'car) 'a 'b))])
             (if (hash-has-key? env (symbol->string sym))
                 (hash-ref env (symbol->string sym))
                 (expr-var (expr-var-type narg) sym '())))
           (expr-app t (expr-intrf s) (list narg)))]

      [(expr-fun name args ret-type body)
       (expr-fun name args ret-type (rp body env))]
      [(expr-lets ts vars vals stmt body)
       (define nvals (map (curryr rp env) vals))
       (define nenv (for/fold [(e env)] ([v vars]) (hash-set e (symbol->string (expr-var-sym v)) v)))
       (expr-lets ts vars nvals (rp stmt nenv) (rp body nenv))]
      [(expr-if t tst thn els)
       (expr-if t (rp tst env) (rp thn env) (rp els env))]
      [(expr-app t (expr-intrf 'index) rands)
       (define nrands (map (curryr rp env) rands))
       ;; (printf "index: ~a\n" (pe (car nrands)))
       (expr-app (second (typeof (car nrands))) (expr-intrf 'index) nrands)]
      [(expr-app t rator rands)
       (expr-app t rator (map (curryr rp env) rands))]
      [(stmt-for i start end b)
       (stmt-for i (rp start env) (rp end env) (rp b env))]
      [(stmt-expr s e)
       (stmt-expr (rp s env) (rp e env))]
      [(stmt-block stmts)
       (stmt-block (map (curryr rp env) stmts))]
      [(stmt-void) (stmt-void)]
      [(stmt-if tst thn els)
       (stmt-if (rp tst env) (rp thn env) (rp els env))]
      [(stmt-assign lhs rhs)
       (stmt-assign (rp lhs env) (rp rhs env))]
      [(expr-var t sym i)
       #:when  (hash-has-key? env (symbol->string sym))
       (hash-ref env (symbol->string sym))]
      [v
       #:when (or (expr-var? v) (expr-val? v))
       v]
      [(? stmt?) (error "stmt not done: ~a\n" (ps e))]
      [(? expr?) (error "expr not done: ~a\n" (pe e))]
      [else (error 'notdone)]))
  (match st
    [(state prgs info os)
     (define nprgs (map (curryr rp (make-immutable-hash)) prgs))
     (run-next nprgs info st)]))

;; we need later-simplifications to run before middle once for removing loops
;; but that also ends up removing let for some loops
(define (fix-loop-lets st)
  (define (apply-inside ast)
    (match ast
      [(expr-sum t i start end b)
       (expr-sum t i start end (pass  b))]
      [(expr-prd t i start end b)
       (expr-prd t i start end (pass  b))]
      [(expr-arr t i end b)
       (expr-arr t i end (pass b))]
      [else ast]))
  (define (pass ast)
    (match ast
      [(expr-fun name args ret-type body)
       (expr-fun name args ret-type (pass body))]
      [(expr-lets ts vars vals stmt body)
       (expr-lets ts vars (map apply-inside vals) (pass stmt) (pass body))]

      [(expr-if t tst thn els)
       (expr-if t (pass tst) (pass thn) (pass els))]

      [(expr-app t rator rands)
       (expr-app t rator (map pass rands))]

      ;; stmt simplifications
      [(stmt-for i start end b)
       (stmt-for i start end (pass b))]

      [(stmt-expr s e)
       (stmt-expr (pass s) (pass e))]

      [(stmt-block stmts)
       (stmt-block (map pass stmts))]
      [(stmt-void) (stmt-void)]

      [(stmt-if tst thn els)
       (stmt-if (pass tst) (pass thn) (pass els))]

      [(stmt-assign lhs rhs)
       (stmt-assign (pass lhs) (pass rhs))]
      [(stmt-return v)
       (stmt-return v)]

      [(expr-sum t i start end b)
       (define var (expr-var t (gensym^ 'sm) '()))
       (expr-lets (list t) (list var) (list (expr-sum t i start end (pass  b)))
                  (stmt-void)
                  var)]
      [(expr-prd t i start end b)
       (define var (expr-var t (gensym^ 'pr) '()))
       (expr-lets (list t) (list var) (list (expr-prd t i start end (pass b)))
                  (stmt-void)
                  var)]
      [(expr-arr t i end b)
       (define var (expr-var t (gensym^ 'ar) '()))
       (expr-lets (list t) (list var) (list (expr-arr t i end (pass b)))
                  (stmt-void)
                  var)]
      [(expr-bucket t start end r)
       (define var (expr-var t (gensym^ 'bk) '()))
       (expr-lets (list t) (list var) (list (expr-bucket t start end r))
                  (stmt-void)
                  var)]
      [else ast]))
  (match st
    [(state prgs info os)
     (define nprgs (map pass prgs))
     (run-next nprgs info st)]))
