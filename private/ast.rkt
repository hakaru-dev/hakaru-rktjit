#lang racket

(provide (all-defined-out))


(define (print-expr e)
  (match e
    [(expr-mod main fns)
     `((main ,(pe main))
       ,@(for/list [(fn fns)]
           `(,(car fn) ,(pe (cdr fn)))))]
    [(expr-fun args ret-type body)
     `(function ,(map pe args) ,(pe body))]
    [(expr-var type sym orig) sym]
    [(expr-arr type index size body)
     `(array ,(pe index) ,(pe size) ,(pe body))]
    [(expr-sum type index start end body)
     `(summate (,(pe index) ,(pe start) ,(pe end)) ,(pe body))]
    [(expr-prd type index start end body)
     `(product (,(pe index) ,(pe start) ,(pe end)) ,(pe body))]
    [(expr-bucket type start end reducer)
     `(bucket ,(pe start) ,(pe end) ,(pr reducer))]
    [(expr-bind var body)
     `(/ ,(pe var) -> ,(pe body))]
    [(expr-match type tst branches)
     `(match ,(pe tst) ,@(map pe branches))]
    [(expr-branch pat body)
     `[,(pp pat) ,(pe body)]]
    [(expr-if type tst thn els)
     `(if ,(pe tst) ,(pe thn) ,(pe els))]
    [(expr-app type rator rands)
     `(,(pe rator) ,@(map pe rands))]
    [(expr-let type var val body)
     `(let (,(pe var) ,(pe val)) ,(pe body))]
    [(expr-lets types vars vals body)
     `(let (,@(for/list ( [var vars] [val vals])
                `(,(pe var) ,(pe val))))
        ,(pe body))]
    [(expr-intr s) s]
    [(expr-intrf s) s]
    [(expr-val t v) v]
    [else `(? ,e)]))
(define pe print-expr)
(define (pr red)
  (match red
    [(reducer-split e a b) `(split ,(pe e) ,(pr a) ,(pr b))]
    [(reducer-fanout a b) `(fanout ,(pr a) ,(pr b))]
    [(reducer-add i) `(add ,(pe i))]
    [(reducer-nop) `(nop)]
    [(reducer-index i e b) `(index ,(pe i) ,(pe e) ,(pr b))]))
(define (pp pat)
  (match pat
    [(pat-var) 'var]
    [(pat-true) 'true]
    [(pat-false) 'false]
    [(pat-pair p) `(pair ,(map pp p))]
    [(pat-ident) 'rec]))

(struct expr-mod  (main fns) #:prefab)
(struct expr-fun  (args ret-type body) #:prefab)
(struct expr-let  (type var val body) #:prefab)
(struct expr-lets (types var vals body) #:prefab)
(struct expr-var  (type sym orig) #:prefab)
(struct expr-arr  (type index size body) #:prefab)
(struct expr-sum  (type index start end body) #:prefab)
(struct expr-prd  (type index start end body) #:prefab)
(struct expr-bucket (type start end reducer) #:prefab)
(struct expr-branch (pat body) #:prefab)
(struct expr-match  (type tst branches) #:prefab)
(struct expr-bind (var body) #:prefab)
(struct expr-if   (type tst thn els) #:prefab)
(struct expr-app  (type rator rands) #:prefab)
(struct expr-val  (type v) #:prefab)
(struct expr-intr (sym) #:prefab)
(struct expr-intrf (sym) #:prefab)
(define (f-or e) (λ fs (for/or ([f fs])
                         (fs e))))
(define expr? (λ (e) ((f-or e) expr-mod? expr-fun? expr-let? expr-var? expr-arr?
                         expr-sum? expr-prd? expr-bucket? expr-branch?
                         expr-match? expr-bind? expr-if? expr-app? expr-val?
                         expr-intr? expr-intrf?)))

(struct reducer-split (e a b) #:prefab)
(struct reducer-fanout (a b) #:prefab)
(struct reducer-add (e) #:prefab)
(struct reducer-nop () #:prefab)
(struct reducer-index (n i a) #:prefab)
(define reducer? (λ (r) ((f-or r) reducer-split? reducer-fanout? reducer-add?
                            reducer-nop? reducer-index?)))
(struct pat-true () #:prefab)
(struct pat-false () #:prefab)
(struct pat-pair (p) #:prefab)
(struct pat-var () #:prefab)
(struct pat-ident () #:prefab)
(define pat? (λ (p) ((f-or p) pat-true? pat-false? pat-pair? pat-var? pat-ident?)))
(define internal-ops
  (apply set '(size index recip nat2prob prob2real + * == and < or not
                    categorical)))
(define sum-prod-loops (set 'summate 'product))
(define internal-loop-ops
  (set 'summate 'product 'array))

(define (typeof ast)
  (match ast
    [(expr-fun args ret-type body)
     'fn]
    [(expr-if t tst thn els)
     t]
    [(expr-app t rt rds)
     t]
    [(expr-let t var val b)
     t]
    [(expr-sum t i start end b)
     t]
    [(expr-prd t i start end b)
     t]
    [(expr-arr t i end b)
     t]
    [(expr-match t tst brs)
     t]
    [(expr-bucket t _ _ _)
     t]
    [(expr-val t v)
     t]
    [(expr-intr s)
     '*]
    [(expr-intrf s)
     '!]
    [(expr-var t s o)
     t]
    [(expr-bucket t s e b)
     t]))

(define-syntax (expr/pass stx)
  (syntax-case stx ()
      [(_ mps ...)
       #'(λ (e)
           (letrec
               ((f (λ (e)
                     (define ne (match e mps ... [else e]))
                     (fill-expr-pass f ne))))
             (f e)))]))

(define-syntax (fill-expr-pass stx)
  (syntax-case stx ()
    [(_ f e mps ...)
     #'(match e
         mps ...
         [(expr-mod main fns)
          (expr-mod (f main) (map f fns))]
         [(expr-fun args ret-type body)
          (expr-fun args ret-type (f body))]
         [(expr-let type var val body)
          (expr-let type var (f val) (f body))]
         [(expr-lets types vars vals body)
          (expr-lets types vars (map f vals) (f body))]
         [(expr-arr t i s b)
          (expr-arr t (f i) (f s) (f b))]
         [(expr-sum t i s e b)
          (expr-sum t (f i) (f s) (f e) (f b))]
         [(expr-prd t i s e b)
          (expr-prd t (f i) (f s) (f e) (f b))]
         [(expr-bucket t s e r)
          (expr-bucket t (f s) (f e) r)]
         [(expr-branch pat body)
          (expr-branch pat (f body))]
         [(expr-match t tst brs)
          (expr-match t (f tst) (map f brs))]
         [(expr-bind v b)
          (expr-bind v (f b))]
         [(expr-if t tst thn els)
          (expr-if t (f tst) (f thn) (f els))]
         [(expr-app t ra rs)
          (expr-app t (f ra) (map f rs))]
         [else e])]))

(define-syntax (expr/pass/state stx) ;TODO, FIXME
  (syntax-case stx ()
      [(_ st initial mps ...)
       #'(λ (e)
           (letrec
               ((f (λ (e s)
                     (values
                      (match e
                        [(expr-mod main fns)
                         (define-values (main^ _) (f main s))
                         (define fns^ (map (λ (fn) (call-with-values (λ () (f fn))
                                                                     (λ (e s) e)))
                                           fns))
                         (expr-mod main^ fns^)]
                        [(expr-fun args ret-type body)
                         (define-values (body^ _) (f body s))
                         (expr-fun args ret-type body^)]
                        [(expr-let type var val body)
                         (define-values (val^ _) (f val s))
                         (define-values (body^ _) (f body s))
                         (expr-let type var val^ body^)]
                        [(expr-arr t i start b)
                         (define-values (i^ _) (f i s))
                         (define-values (start^ _) (f start s))
                         (define-values (b^ _) (f b s))
                         (expr-arr t i^ start^ b^)]
                        [(expr-sum t i start end b)
                         (define-values (i^ _) (f i s))
                         (define-values (start^ _) (f start s))
                         (define-values (end^ _) (f end s))
                         (define-values (b^ _) (f b s))
                         (expr-sum t i^ start^ end^ b^)]
                        [(expr-prd t i start end b)
                         (define-values (i^ _) (f i s))
                         (define-values (start^ _) (f start s))
                         (define-values (end^ _) (f end s))
                         (define-values (b^ _) (f b s))
                         (expr-prd t i^ start^ end^ b^)]
                        [(expr-bucket t siz e r)
                         (define-values (siz^ _) (f siz s))
                         (define-values (e^ _) (f e s))
                         (expr-bucket t siz^ e^ r)]
                        [(expr-branch pat body)
                         (define-values (body^ _) (f body s))
                         (expr-branch pat body^)]
                        [(expr-match t tst brs)
                         (define-values (tst^ _) (f tst s))
                         (define brs^ (map (λ (br) (call-with-values (λ () (f br))
                                                                     (λ (e s) e)))
                                           brs))
                         (expr-match t tst^ brs^)]
                        [(expr-bind v b)
                         (define-values (b^ _) (f b s))
                         (expr-bind v b^)]
                        [(expr-if t tst thn els)
                         (define-values (tst^ _) (f tst s))
                         (define-values (thn^ _) (f thn s))
                         (define-values (els^ _) (f els s))
                         (expr-if t tst^ thn^ els^)]
                        [(expr-app t ra rs)
                         (define-values (ra^ _) (f ra s))
                         (define rs^ (map (λ (rs) (call-with-values (λ () (f rs))
                                                                     (λ (e s) e)))
                                           rs))
                         (expr-app t ra^ rs^)]
                        [else e])
                      st))))
             (f^ initial e)))]))
