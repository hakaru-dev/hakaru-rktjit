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
  (or (symbol? x) (number? x) ;; (and (not (set-member? internal-loop-ops (car x)))
                              ;;      (andmap simple? x))
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
          (let1 (ret ,(do-anf body))
                ret))]
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
      ['== (values '? '(?))]
      ['and (values '? '(?))]
      ['or (values '? '(?))]
      ['< (values '? '(?))]

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

(define (reduce-to-folds body)
  (define rh reduce-to-folds)
  (define (init-value op)
    (match op
      ['summate 0]
      ['product 1]))
  (define (get-assign op result index body)
    (match op
      ['summate `(set! ,result (+ ,result ,body))]
      ['product `(set! ,result (* ,result ,body))]))
  (define (make-array t)
    (symbol-append 'make-array- (cadr t)))
  (match body
    [`(array ,type (,index ,size) ,body)
     (define iv (gensym^ 'iv))
     (define result (gensym^ 'r))
     `(fold-loop (,index 0 ,size)
                 (,result (,(make-array type) ,size) ,(get-type type))
                 (let ((,iv ,(rh body) ,(cadr type)))
                   (set! (index ,result ,index) ,iv)))]
    [`(,op (,index ,start ,end) ,body) #:when (set-member? internal-loop-ops op)
     (define result (gensym^ 'r))
     `(fold-loop (,index ,start ,end)
                 (,result ,(init-value op) real)
                 ,(get-assign op result index (rh body)))]
    [`(if ,tst ,thn ,els)
     `(if ,(rh tst) ,(rh thn) ,(rh els))]
    [`(,l ((,args ,vals ,types) ...) ,body) #:when (set-member? (set 'let 'let*) l)
     `(,l ,(for/list ([arg args] [val vals] [type types])
              `(,arg ,(rh val) ,type)) ,(rh body))]
    [`(fn ,args ,ret-type ,body)
     `(fn ,args ,ret-type ,(rh body))]
    [`(,rands ...)
     `(,@(map rh rands))]
    [else body]))

(define (reduce-folds body)
  (define rf reduce-folds)
  (match body
    [`(fold-loop (,index ,start ,end)
                 (,result ,init-value ,result-type)
                 ,body)
     `(let ((,result ,init-value ,result-type))
        (begin
          (for ((,index ,start) (< ,index ,end) (+ ,index 1))
            ,(rf body))
          ,result))]
    [`(if ,tst, thn ,els)
     `(if ,(rf tst) ,(rf thn) ,(rf els))]
    [`(fn ,args ,ret-type ,body)
     `(fn ,args ,ret-type ,(rf body))]
    [`(,rands ...)
     `(,@(map rf rands))]
    [else body]))

(define (get-type tast)
  (match tast
    [`(array ,t)
     (symbol-append (symbol-append 'array- t) '-p)]
    [else tast]))
(define op-map (make-hash '((+ . jit-add)
                            (* . jit-mul)
                            (< . jit-lt?))))
(define (compile-to-jit-lang body)
  (define (check-and-assign body assign-to)
    (if assign-to
        `(set! ,assign-to ,(jitfy body))
        body))
  (define (jitfy expr)
    (match expr
      [(? number?) `(#%value ,expr nat)]
      [else (hash-ref op-map expr expr)]))
  (define (ctj body assign-to)
    (match body
      [`(let ((,vars ,vals ,types) ...) ,body)
       `(let ,(map (λ (v t) `(,v : ,t)) vars types)
          (block
           ,@(map (λ (var val) (ctj val var)) vars vals)
           ,(ctj body assign-to)))]
      [`(begin ,exps ... ,end-exp)
       `(block ,@(map (curryr ctj #f) exps)
               ,(ctj end-exp assign-to))]
      [`(for ((,index ,init-value) ,check ,next-value) ,body)
       `(let ((,index : nat))
          (block
           ,(ctj init-value index)
           (while ,(ctj check #f)
             (block ,(ctj body assign-to)
                    ,(ctj next-value index)))))]
      [`(index ,arr ,i)
       `(#%app ,(string->symbol (format "index-array-~a"
                                        (cadr (hash-ref arg-types arr '(array real))))) ,arr ,i)]
      [`(size ,arr)
       `(#%app ,(string->symbol (format "size-array-~a"
                                        (cadr (hash-ref arg-types arr '(array real))))) ,arr)]
      [`(recip ,v)
       `(#%app jit-div (#%value 1.0 real) ,(ctj v #f))]
      [`(,rator ,rands ...)
       (check-and-assign `(,@(if (eq? rator 'set!) `(,rator)  `(#%app ,(jitfy rator)))
                           ,@(map (curryr ctj #f) rands)) assign-to)]
      [else (check-and-assign (jitfy body) assign-to)]))

  (define arg-types (match body
    [`(fn ,args ,ret-type (let ((ret ,body ,type)) ret))
     (for/hash ([arg args])
       (values (car arg) (cadr arg)))]))
  (match body
    [`(fn ,args ,ret-type (let ((ret ,body ,t)) ret))
     `(module
          ,@(basic-defines)
          (define-function (f ,@(for/list ([arg args])
                                `(,(car arg) : ,(get-type (cadr arg))))
                            : ,(get-type ret-type))
            (let ((ret : ,(get-type ret-type)))
              (block
               ,(ctj body
                     'ret)
               (return ret)))))]))

(define compilers (list reduce-function
                        simplify-exp
			uniquify 
                        do-anf
                        combine-lets
                        assign-types
                        reduce-to-folds
                        reduce-folds
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
                      '(begin let lambda set! do))])
       (pretty-display prg))
     (printf "\n\napplying ~a\n" (object-name c))
     (c prg)))
  (pretty-display prog-ast)
  (define env (compile-module prog-ast))
  env
  
  )

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

  
  (debug-program nbg-src compilers)
  )
