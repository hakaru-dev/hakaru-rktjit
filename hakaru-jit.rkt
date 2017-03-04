#lang racket

(require "../libjit/jit.rkt")
(require "../libjit/jit-utils.rkt")
(require "basic-defines.rkt")

(define (read-file filename)
  (call-with-input-file filename
    (lambda (in)
      (read in))))

(struct function (args ret-type body))
(struct variable (type sym))
(struct array (type index size body))
(struct summate (type index start end body))
(struct product (type index start end body))
(struct ifexp (type tst thn els))
(struct appexp (type rator rands))

(define internal-ops
  (apply set '(size index recip nat2prob prob2real + * == && < ||)))
(define sum-prod-loops (set 'summate 'product))
(define internal-loop-ops
  (set 'summate 'product 'array))

(define (reduce-function exp)
  (define (combine-functions prg [var-type-assoc null])
    (match prg
      [`((fn ,var ,type ,body) : ,t)
       (combine-functions body (cons (list var (car t)) var-type-assoc))]
      [else (values (reverse var-type-assoc) prg)]))
  (define-values (args fbody) (combine-functions exp))
  (match fbody
    [`(,body : ,type)
     `(fn ,args ,type ,fbody)]))

(define (simplify-exp exp)
  (define sa simplify-exp)
  (match exp
    [`(fn ,args ,ret-type ,body)
     `(fn ,args ,ret-type ,(sa body))]
    [`((summate (,index ,start ,end) ,body) : ,_)
     `(summate (,index ,(sa start) ,(sa end)) ,(sa body))]
    [`((product (,index ,start ,end) ,body) : ,_)
     `(product (,index ,(sa start) ,(sa end)) ,(sa body))]
    [`((array (,index ,size) ,body) : ,type)
     `(array ,type (,index ,(sa size)) ,(sa body))]
    [`((match ,tst (true ,thn) (false ,els)) : ,_)
     `(if ,(sa tst) ,(sa thn) ,(sa els))]
    [`((,rator ,rands ...) : ,_)
     `(,rator ,@(map sa rands))]
    [`(,s : ,_)
     s]
    [else exp]))

(define (uniquify exp)
  (define (rv exp vars)
    (match exp
      [(? symbol?)
       (cdr (assoc exp vars))]
      [(? number?)
       exp]
      [`(,rator ,rands ...)
       `(,rator ,(map (curryr u vars) rands))]))
  (define (u exp vars)
    (match exp
      [`(fn ,args ,ret-type ,body)
       `(fn ,args ,ret-type
                  ,(u body
                      (map (λ (at) (cons (car at) (car at))) args)))]
      [`(,op (,index ,start ,end) ,body) #:when (set-member? sum-prod-loops op)
       (define i (gensym^ index))
       (define nv (cons (cons index i) vars))
       `(,op (,i ,(u start vars) ,(u end vars)) ,(u body nv))]
      [`(array ,type (,index ,size) ,body)
       (define i (gensym^ index))
       (define nv (cons (cons index i) vars))
       `(array ,type (,i ,(u size vars)) ,(u body nv))]
      [`(if ,tst ,thn ,els)
       `(if ,(u tst vars) ,(u thn vars) ,(u els vars))]
      [`(,rator ,rands ...)
       `(,rator ,@(map (curryr u vars) rands))]
      [else (rv exp vars)]))
  (u exp '()))

(define (simple? x)
  (or (symbol? x) (number? x)
      (and (not (set-member? internal-loop-ops (car x)))
           (andmap simple? x))
      ))

(define (do-anf expr)
  (define (wrap-simplify body)
    (if (simple? body)
        (values body identity)
        (let ((sym (gensym^ 's)))
          (values sym
                  (λ (prg) `(let1 (,sym ,(do-anf body)) ,prg))))))
  (match expr
    [`(fn ,args ,ret-type ,body)
     `(fn ,args ,ret-type
          ,(do-anf body))]
    [`(,op (,index ,start ,end) ,body)
     #:when (set-member? sum-prod-loops op)
     (define-values (start-sym start-wrap) (wrap-simplify start))
     (define-values (end-sym end-wrap) (wrap-simplify end))
     (start-wrap (end-wrap `(,op (,index ,start-sym ,end-sym) ,(do-anf body))))]
    [`(array ,type (,index ,size) ,body)
     (define-values (size-sym size-wrap) (wrap-simplify size))
     (size-wrap `(array ,type (,index ,size-sym) ,(do-anf body)) )]
    [`(if ,tst ,thn ,els)
     (define-values (tst-sym tst-wrap) (wrap-simplify tst))
     (tst-wrap `(if ,tst-sym ,(do-anf thn) ,(do-anf els)))]
    [`(,exprs ...)
     (define-values (syms wrappers)
       (for/fold [(syms '())
                  (wrappers '())]
                 [(e (reverse exprs))]
         (define-values (s w) (wrap-simplify e))
         (values (cons s syms) (cons w wrappers))))
     ;(printf "syms: ~a\n" syms)
     (for/fold [(e syms)]
               [(w wrappers)]
       (w e))]
    [(? simple?) expr]))

(define (combine-lets expr)
  (define (get-let-map expr let-map)
    (match expr
      [`(let1 (,sym ,val) ,body)
       (define-values (val-body val-let-map) (get-let-map val let-map))
       (define nlet-map (cons (cons sym val-body) val-let-map))
       (get-let-map body nlet-map)]
      [`(array ,type (,index ,size) ,body)
       (values `(array ,type (,index ,size) ,(combine-let1 body)) let-map)]
      [`(,op (,index ,start ,end) ,body)
       #:when (set-member? internal-loop-ops op)
       (values `(,op (,index ,start ,end) ,(combine-let1 body)) let-map)]
      [`(if ,tst ,thn ,els)
       (values `(if ,(combine-let1 tst) ,(combine-let1 thn) ,(combine-let1 els))
               let-map)]
      [else
       (values expr let-map)]))
  (define (combine-let1 expr)
    (define-values (body-expr body-let-map) (get-let-map expr '()))
    ;; (printf "body-expr:\n\t ~a\n body-let-map: \n\t~a\n" body-expr body-let-map)
    (if (not (null? body-let-map))
        `(,(if (eq? (length body-let-map) 1) 'let 'let*)
             ,(for/list [(s (reverse body-let-map))]
                `(,(car s) ,(cdr s)))
           ,body-expr)
        body-expr))
  (match expr
    [`(fn ,args ,ret-type ,body)
     `(fn ,args ,ret-type ,(combine-let1 body))]))

(define (assign-types expr)
  (define (unify-type t1 t2)
    (define (nat? t) (eq? t 'nat))
    (define (real? t) (eq? t 'real))
    (define (array-type? t) (and (cons? t) (eq? (car t) 'array)))
    (define (nat-or-real? t) (or (nat? t) (real? t)))
    (define (t? t) (eq? t '?))
    (cond [(eq? t1 t2) t1]
          [(t? t1) t2]
          [(t? t2) t1]
          [(and (real? t1) (not (array-type? t2))) t1]
          [(and (real? t2) (not (array-type? t1))) t2]
          [(and (array-type? t1) (array-type? t2)) (list 'array
                                                         (unify-type (cadr t1) (cadr t2)))]
          [else (error 'unifying-type t1 t2)]))
  (define (jit-type t)
    (if (symbol? t) t (string->symbol (format "~a-~a-p" (car t) (cadr t)))))
  (define (get-function-type op n)
    (match op
      [(? (λ (o) (set-member? (set '+ '* '-) o)))
       (values 'real (make-list n 'real))]
      ['prob2real (values 'real '(real))]
      ['recip (values 'real '(real))]
      ['nat2prob (values 'real '(nat))]
      ['== (values 'nat '(? ?))]
      ['and (values 'nat '(? ?))]
      ['or (values 'nat '(? ?))]
      ['< (values 'nat '(? ?))]

      ['size (values 'nat '((array ?)))]))
  (define (get-arg-types arglist)
    (match arglist
      [`((,args ,types) ...)
       (for/hash ([a args] [t types])
         (values a t))]))
  (define (get-and-assign-types expr type type-env)
    (define gast get-and-assign-types)
    (match expr
      [`(array ,type (,index ,size) ,body)
       (define-values (new-body body-type new-type-env)
         (gast body (cadr type) (hash-set type-env index 'nat)))
       (values `(array ,type (,index ,size) ,new-body)
               type
               new-type-env)]
      [`(,op (,i ,iv ,ev) ,body) #:when (set-member? (set 'array 'summate 'product) op)
       (define-values (new-body body-type new-type-env)
         (gast body '? (hash-set type-env i 'nat)))
       (values `(,op (,i ,iv ,ev) ,new-body)
               (if (eq? op 'array) `(array ,body-type) body-type)
               new-type-env)]
      [`(let ((,s ,v)) ,body)
       (define-values (n-v v-t v-t-env) (gast v '? (hash-set type-env s '?)))
       (define-values (n-b b-t b-t-env) (gast body type v-t-env))
       (values `(let ((,s ,n-v ,(jit-type v-t))) ,n-b)
               b-t
               b-t-env)]
      [`(let* ((,syms ,vals) ...) ,body)
       (define-values (val-bodys val-types val-env)
         (for/fold ([new-vals '()]
                    [val-types '()]
                    [val-env type-env])
                   ([s syms]
                    [v vals])
           (define-values (new-val val-type new-env) (gast v '? val-env))
           ;; (printf "let* s: ~a, v: ~a, new-val: ~a, val-type: ~a\n" s v new-val val-type)
           (values (append new-vals (list new-val))
                   (append val-types (list val-type))
                   (hash-set new-env s val-type))))
       (define-values (new-body body-type body-env) (gast body type val-env))
       (values `(let* ,(for/list ([s syms]
                                  [v val-bodys]
                                  [t val-types])
                         `(,s ,v ,(jit-type t)))
                  ,new-body)
               body-type
               body-env)]
      [`(if ,tst ,thn ,els)
       (define-values (thn-b thn-t thn-env) (gast thn type type-env))
       (define-values (els-b els-t els-env) (gast els thn-t thn-env))
       (values `(if ,tst ,thn-b ,els-b)
               els-t
               els-env)]
      [`(index ,arr ,i)
       (define arr-type (hash-ref type-env arr))
       (values `(index ,arr ,i)
               (cadr arr-type)
               (hash-set type-env i 'nat))]
      [`(,rator ,rands ...)
       (define-values (ret-type arg-types) (get-function-type rator (length rands)))
       (define-values (new-type-env new-rands)
         (for/fold ([type-env type-env]
                    [rnds '()])
                   ([r rands]
                    [t arg-types])
           (define-values (new-r r-type new-type-env) (gast r t type-env))
           (values new-type-env (cons new-r rnds))))
       (values `(,rator ,@new-rands)
               ret-type
               new-type-env)]
      [(? symbol?)
       (define t (unify-type type (hash-ref type-env expr)))
       (values expr t (hash-set type-env expr t))]
      [(? number?)
       (define t (unify-type type 'nat))
       (values expr t type-env)]))
  (match expr
    [`(fn ,args ,ret-type ,body)
     (define-values (new-body body-type type-env)
       (get-and-assign-types body ret-type (get-arg-types args)))
     `(fn ,args ,ret-type ,new-body)]))

;; (define (reduce-to-folds body)
;;   (define rh reduce-to-folds)
;;   (define (init-value op)
;;     (match op
;;       ['summate 0]
;;       ['product 1]))
;;   (define (get-assign op result index body)
;;     (match op
;;       ['summate `(set! ,result (+ ,result ,body))]
;;       ['product `(set! ,result (* ,result ,body))]))
;;   (define (make-array t)
;;     (symbol-append 'make-array- (cadr t)))
;;   (match body
;;     [`(array ,type (,index ,size) ,body)
;;      (define iv (gensym^ 'iv))
;;      (define result (gensym^ 'r))
;;      `(fold-loop (,index 0 ,size)
;;                  (,result (,(make-array type) ,size) ,(get-type type))
;;                  (let ((,iv ,(rh body) ,(cadr type)))
;;                    (set! (index ,result ,index) ,iv)))]
;;     [`(,op (,index ,start ,end) ,body) #:when (set-member? internal-loop-ops op)
;;      (define result (gensym^ 'r))
;;      `(fold-loop (,index ,start ,end)
;;                  (,result ,(init-value op) real)
;;                  ,(get-assign op result index (rh body)))]
;;     [`(if ,tst ,thn ,els)
;;      `(if ,(rh tst) ,(rh thn) ,(rh els))]
;;     [`(,l ((,args ,vals ,types) ...) ,body) #:when (set-member? (set 'let 'let*) l)
;;      `(,l ,(for/list ([arg args] [val vals] [type types])
;;               `(,arg ,(rh val) ,type)) ,(rh body))]
;;     [`(fn ,args ,ret-type ,body)
;;      `(fn ,args ,ret-type ,(rh body))]
;;     [`(,rands ...)
;;      `(,@(map rh rands))]
;;     [else body]))

;; (define (reduce-folds body)
;;   (define rf reduce-folds)
;;   (match body
;;     [`(fold-loop (,index ,start ,end)
;;                  (,result ,init-value ,result-type)
;;                  ,body)
;;      `(let ((,result ,init-value ,result-type))
;;         (begin
;;           (for ((,index ,start) (< ,index ,end) (+ ,index 1))
;;             ,(rf body))
;;           ,result))]
;;     [`(if ,tst, thn ,els)
;;      `(if ,(rf tst) ,(rf thn) ,(rf els))]
;;     [`(fn ,args ,ret-type ,body)
;;      `(fn ,args ,ret-type ,(rf body))]
;;     [`(,rands ...)
;;      `(,@(map rf rands))]
;;     [else body]))

(define (flatten expr)
  (define (assign body to)
    (match body
      [`(,l ((,syms ,vals ,types) ...) ,b) #:when (set-member? (set 'let 'let*) l)
       `(let ,(for/list [(s syms)
                          (t types)]
                 `(,s : ,t))
          (block
           ,@(for/list ([v vals]
                        [s syms])
               (assign v s))
           ,(assign b to)))]
      [`(summate (,i ,start ,end) ,b)
       (define res (gensym^ 'sr))
       (define t (gensym^ 'st))
       `(let ((,res : real)
              (,t : real))
          (block
           (set! ,res 0)
           (for1 ((,i ,start (+ ,i 1) : nat) (< ,i ,end))
                 (block
                  ,(assign b t)
                  (set! ,res (* ,res ,t))))
           (set! ,to ,res)))]
      [`(product (,i ,start ,end) ,b)
       (define res (gensym^ 'pr))
       (define t (gensym^ 'pt))
       `(let ((,res : real)
              (,t : real))
          (block
           (set! ,res 1)
           (for1 ((,i ,start (+ ,i 1) : nat) (< ,i ,end))
                 (block
                  ,(assign b t)
                  (set! ,res (* ,res ,t))))
           (set! ,to ,res)))]
      [`(array (array ,typ) (,i ,i-end) ,b)
       (define ai (gensym^ 'a))
       `(block
         (set! ,to (,(string->symbol (format "empty-~a-array" typ))  ,i-end))
         (for1 ((,i 0 (+ ,i 1) : nat) (< ,i ,i-end))
               (let ((,ai : ,typ))
                 (block
                  ,(assign b ai)
                  (,(string->symbol (format "set-array-~a-at-index!" typ)) ,to ,i ,ai)))))]
      [`(if ,tst ,thn ,els)
       `(if ,tst ,(assign thn to) ,(assign els to))]
      [else `(set! ,to ,body)]))
  (match expr
    [`(fn ,args ,ret-type ,body)
     `(fn ,args ,ret-type
          (let ((ret : ,ret-type))
            (block
             ,(assign body 'ret)
             (return ret))))]))

(define (get-type tast)
  (match tast
    [`(array ,t)
     (symbol-append (symbol-append 'array- t) '-p)]
    [else tast]))
(define op-map (make-hash '((+ . jit-add)
                            (* . jit-mul)
                            (< . jit-lt?)
                            (== . jit-eq?)
                            (and . jit-and)
                            (or . jit-or)
                            (recip . recip-real)
                            )))
(define (compile-to-jit-lang body)
  (define (ce expr arg-types)
    (match expr
      [(? number?)
       `(#%value ,expr nat)]
      [`(size ,arr)
       (define arr-type (hash-ref arg-types arr))
       `(#%app ,(symbol-append 'size-array- (cadr arr-type)) ,arr)]
      [`(index ,arr ,i)
       (define arr-type (hash-ref arg-types arr))
       `(#%app ,(symbol-append 'index-array- (cadr arr-type)) ,arr ,(ce i arg-types))]
      [`(,rator ,rands ...)
       `(#%app ,(hash-ref op-map rator rator) ,@(map (curryr ce arg-types) rands))]
      [(? symbol?)
       expr]))
  (define (cs body arg-types)
    (match body
      [`(let ((,args : ,tys) ...) ,body)
       `(let ,(for/list ([arg args]
                         [ty tys])
                `(,arg : ,(get-type ty)))
          ,(cs body arg-types))]
      [`(block ,exps ...)
       `(block ,@(map (curryr cs arg-types) exps))]
      [`(for1 ((,i ,start ,next : ,ty) ,tst) ,body)
       `(let ((,i : ,ty))
          (block
           (set! ,i ,(ce start arg-types))
           (while ,(ce tst arg-types)
             (block
              ,(cs body arg-types)
              (set! ,i ,(ce next arg-types))))))]
      [`(if ,tst ,thn ,els)
       `(if ,(ce tst arg-types)
            ,(cs thn arg-types)
            ,(cs els arg-types))]
      [`(set! ,a ,e)
       `(set! ,a ,(ce e arg-types))]
      [`(set-array-prob-at-index! ,arr ,i ,v)
       `(#%exp (#%app set-array-prob-at-index! ,arr ,(ce i arg-types) ,(ce v arg-types)))]
      [`(return ,v)
       `(return ,v)]
      [else (error "unknown expression in compile-to-jit" body)]))
  (match body
    [`(fn ,args ,ret-type ,body)
     (define arg-types (for/hash ([arg args])
                         (values (car arg) (cadr arg))))
     `(module
          ,@(basic-defines)
          (define-function (f ,@(for/list ([arg args])
                                `(,(car arg) : ,(get-type (cadr arg))))
                            : ,(get-type ret-type))
            ,(cs body arg-types)))]))

(define compilers (list reduce-function
                        simplify-exp
			uniquify 
                        do-anf
                        combine-lets
                        assign-types
                        flatten
                        ;; reduce-to-folds
                        ;; reduce-folds
                        compile-to-jit-lang
                        ))

(define (debug-program prg cmplrs)
  (define prog-ast
   (for/fold ([prg prg])
             ([c cmplrs])
     (parameterize ([pretty-print-current-style-table
                     (pretty-print-extend-style-table
                      (pretty-print-current-style-table)
                      '(block define-variables define-function assign while)
                      '(begin let lambda set! do))]
                    [pretty-print-columns 100])
       (pretty-display prg))
     (printf "\n\napplying ~a\n" (object-name c))
     (c prg)))
  (pretty-display prog-ast)
  (define env (compile-module prog-ast))
  env)

(module+ test
  (require ffi/unsafe)
  (define hello-src (read-file "examples/hello-full.hkr"))
  (define nbg-src (read-file "examples/naive-bayes-gibbs-full.hkr"))
  ;; (debug-program hello-src compilers)
  ;; (define hello-env (debug-program hello-src compilers))
  ;; (define (get-hello-f f)
  ;;   (jit-get-function (env-lookup f hello-env)))
  ;; (define real-type (jit-get-racket-type (env-lookup 'real hello-env)))
  ;; (define test-array ((get-hello-f 'make-array-real)  4
  ;;                     (list->cblock `(80.0 20.1 30.2 40.4 ) real-type)))


  ;; (printf "test value: ~a\n"((get-hello-f 'f) test-array))

  

  (jit-dump-module  (debug-program nbg-src compilers) )
  )

;; (compile-to-jit-lang(flatten '(fn
;;                ((topic_prior (array prob))
;;                 (word_prior (array prob))
;;                 (z (array nat))
;;                 (w (array nat))
;;                 (doc (array nat))
;;                 (docUpdate nat))
;;              (array prob)
;;              (let* ((s1 (size topic_prior) nat))
;;                (array
;;                 (array prob)
;;                 (zNew1 s1)
;;                 (let ((s2 (size topic_prior) nat))
;;                   (product
;;                    (k1 0 s2)
;;                    (let ((s3 (size word_prior) nat))
;;                      (product
;;                       (i1 0 s3)
;;                       (let* ((s68 (size w) nat)
;;                              (s4
;;                               (summate
;;                                (j2 0 s68)
;;                                (let* ((s74 (index doc j2) nat)
;;                                       (s69 (== docUpdate s74) nat))
;;                                  (if s69
;;                                      (let* ((s73 (index w j2) nat)
;;                                             (s71 (== s73 i1) nat)
;;                                             (s72 (== zNew1 k1) nat)
;;                                             (s70 (and s71 s72) nat))
;;                                        (if s70 1 0))
;;                                      0)))
;;                               nat))
;;                         (product
;;                          (j1 0 s4)
;;                          (let* ((s39 (size topic_prior) nat)
;;                                 (s38
;;                                  (product
;;                                   (k2 0 s39)
;;                                   (let* ((s58 (size w) nat)
;;                                          (s40
;;                                           (summate
;;                                            (j8 0 s58)
;;                                            (let* ((s67 (index doc j8) nat)
;;                                                   (s59 (== docUpdate s67) nat))
;;                                              (if s59
;;                                                  (let* ((s61 (== zNew1 k2) nat)
;;                                                         (s66 (index w j8) nat)
;;                                                         (s63 (== s66 0) nat)
;;                                                         (s65 (index w j8) nat)
;;                                                         (s64 (< s65 0) nat)
;;                                                         (s62 (or s63 s64) nat)
;;                                                         (s60 (and s61 s62) nat))
;;                                                    (if s60 1 0))
;;                                                  0)))
;;                                           nat))
;;                                     (product
;;                                      (j7 0 s40)
;;                                      (let* ((s57 (size word_prior) nat)
;;                                             (s41
;;                                              (summate
;;                                               (j10 0 s57)
;;                                               (index word_prior j10))
;;                                              prob)
;;                                             (s42 (nat2prob j7) real)
;;                                             (s45 (size w) nat)
;;                                             (s44
;;                                              (summate
;;                                               (j9 0 s45)
;;                                               (let* ((s56 (index doc j9) nat)
;;                                                      (s46 (== docUpdate s56) nat))
;;                                                 (if s46
;;                                                     0
;;                                                     (let* ((s55 (index doc j9) nat)
;;                                                            (s54 (index z s55) nat)
;;                                                            (s48 (== s54 k2) nat)
;;                                                            (s53 (index w j9) nat)
;;                                                            (s50 (== s53 0) nat)
;;                                                            (s52 (index w j9) nat)
;;                                                            (s51 (< s52 0) nat)
;;                                                            (s49 (or s50 s51) nat)
;;                                                            (s47 (and s48 s49) nat))
;;                                                       (if s47 1 0)))))
;;                                              nat)
;;                                             (s43 (nat2prob s44) real))
;;                                        (+ s41 s42 s43)))))
;;                                  real)
;;                                 (s5 (recip s38) real)
;;                                 (s37 (size topic_prior) nat)
;;                                 (s30
;;                                  (summate (j6 0 s37) (index topic_prior j6))
;;                                  prob)
;;                                 (s33 (size z) nat)
;;                                 (s32
;;                                  (summate
;;                                   (j5 0 s33)
;;                                   (let ((s34 (== docUpdate j5) nat))
;;                                     (if s34
;;                                         0
;;                                         (let* ((s36 (index z j5) nat)
;;                                                (s35 (< 0 s36) nat))
;;                                           (if s35 0 1)))))
;;                                  nat)
;;                                 (s31 (nat2prob s32) real)
;;                                 (s29 (+ s30 s31) real)
;;                                 (s6 (recip s29) real)
;;                                 (s22 (index topic_prior zNew1) prob)
;;                                 (s25 (size z) nat)
;;                                 (s24
;;                                  (summate
;;                                   (j4 0 s25)
;;                                   (let ((s26 (== docUpdate j4) nat))
;;                                     (if s26
;;                                         0
;;                                         (let* ((s28 (index z j4) nat)
;;                                                (s27 (== s28 zNew1) nat))
;;                                           (if s27 1 0)))))
;;                                  nat)
;;                                 (s23 (nat2prob s24) real)
;;                                 (s7 (+ s22 s23) real)
;;                                 (s9 (index word_prior i1) prob)
;;                                 (s10 (nat2prob j1) real)
;;                                 (s13 (size w) nat)
;;                                 (s12
;;                                  (summate
;;                                   (j3 0 s13)
;;                                   (let* ((s21 (index doc j3) nat)
;;                                          (s14 (== docUpdate s21) nat))
;;                                     (if s14
;;                                         0
;;                                         (let* ((s20 (index w j3) nat)
;;                                                (s16 (== s20 i1) nat)
;;                                                (s19 (index doc j3) nat)
;;                                                (s18 (index z s19) nat)
;;                                                (s17 (== s18 k1) nat)
;;                                                (s15 (and s16 s17) nat))
;;                                           (if s15 1 0)))))
;;                                  nat)
;;                                 (s11 (nat2prob s12) real)
;;                                 (s8 (+ s9 s10 s11) real))
;;                            (* s5 s6 s7 s8)))))))))))))
