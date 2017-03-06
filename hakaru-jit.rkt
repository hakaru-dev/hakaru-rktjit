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
     ;; (parameterize ([pretty-print-current-style-table
     ;;                 (pretty-print-extend-style-table
     ;;                  (pretty-print-current-style-table)
     ;;                  '(block define-variables define-function assign while)
     ;;                  '(begin let lambda set! do))]
     ;;                [pretty-print-columns 100])
     ;;   (pretty-display prg))
     ;; (printf "\n\napplying ~a\n" (object-name c))
     (c prg)))
  ;; (pretty-display prog-ast)
  (compile-module prog-ast))

(module+ test
  (require ffi/unsafe)
  (define hello-src (read-file "examples/hello-full.hkr"))
  (define nbg-src (read-file "examples/naive-bayes-gibbs-full.hkr"))
  ;; (debug-program hello-src compilers)
  ;; (define hello-env (debug-program hello-src compilers))
  ;; (define (get-hello-f f)
  ;;   (jit-get-function (env-lookup f hello-env)))
  ;; (define test-array ((get-hello-f 'make-array-real)  4
  ;;                     (list->cblock `(80.0 20.1 30.2 40.4 ) real-type)))


  ;; (printf "test value: ~a\n"((get-hello-f 'f) test-array))

  
  (define nbg-env (debug-program nbg-src compilers))
  ;; (jit-dump-module   nbg-env)
  (define (get-nbg-f f)
    (jit-get-function (env-lookup f nbg-env)))
  (define prob-type (jit-get-racket-type (env-lookup 'prob nbg-env)))
  (define nat-type (jit-get-racket-type (env-lookup 'nat nbg-env)))

  (define make-array-prob (get-nbg-f 'make-array-prob))
  (define make-array-nat (get-nbg-f 'make-array-nat))

  (define topic-prior (make-array-prob 100 (list->cblock '(0.014228 0.003821 0.030999 0.002363 0.024379 0.001317 0.002707 0.013426 0.000219 0.008970 0.012040 0.003862 0.006986 0.012392 0.004347 0.003219 0.011661 0.006158 0.011185 0.060091 0.008681 0.000573 0.019064 0.010391 0.005424 0.022017 0.015388 0.001420 0.023657 0.007121 0.003429 0.004026 0.005844 0.003225 0.045181 0.006403 0.018045 0.006239 0.008657 0.000347 0.002385 0.000023 0.005600 0.032088 0.005185 0.014991 0.003511 0.001131 0.004627 0.002341 0.013252 0.016136 0.017101 0.003137 0.003582 0.000282 0.000733 0.018463 0.004997 0.000185 0.029280 0.005629 0.018106 0.016644 0.013107 0.002684 0.019586 0.004801 0.020799 0.008960 0.013165 0.000553 0.000053 0.001443 0.000540 0.008137 0.005964 0.004829 0.005559 0.004122 0.000518 0.018653 0.022602 0.007208 0.007697 0.037427 0.002005 0.002432 0.012954 0.000968 0.001409 0.005069 0.019474 0.015708 0.001842 0.009659 0.009836 0.010813 0.029945 0.000568 ) prob-type)))
  (define word-prior (make-array-prob 3 (list->cblock '(0.023754 0.277952 0.698294) prob-type)))
  (define z (make-array-nat 20 (list->cblock '(1 0 2 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 ) nat-type)))
  (define w (make-array-nat 120 (list->cblock '(40 0 14 0 55 0 68 0 50 0 67 0 44 0 31 0 89 0 66 0 29 0 43 0 57 0 69 0 75 0 77 0 44 0 39 0 40 0 97 0 53 0 96 0 43 0 26 0 36 0 8 0 78 0 94 0 82 0 8 0 77 0 55 0 7 0 86 0 42 0 95 0 10 0 94 0 22 0 73 0 30 0 88 0 14 0 90 0 18 0 47 0 52 0 26 0 30 0 76 0 74 0 92 0 20 0 63 0 50 0 74 0 51 0 81 0 76 0 60 0 ) nat-type)))
  (define doc (make-array-nat 120 (list->cblock '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 2 0 2 0 2 0 2 0 2 0 2 0 2 0 2 0 2 0 2 0 2 0 2 0 2 0 2 0 2 0 2 0 2 0 2 0 2 0 2 0 ) nat-type)))
  (define f (get-nbg-f 'f))
  (define result_raw (time (f topic-prior word-prior z w doc 1)))
  (define get-array-prob (get-nbg-f 'get-array-prob))
  (define result (cblock->list (get-array-prob result_raw) prob-type 120))
  (pretty-display result)
  )
