#lang racket

(require "../racket-jit/jit.rkt")
(require "../racket-jit/jit-utils.rkt")
(require "basic-defines.rkt")

(define (read-file filename)
  (call-with-input-file filename
    (lambda (in)
      (read in))))
(define (print-expr e)
  (define pe print-expr)
  (match e
    [(expr-fun args ret-type body)
     `(function ,(map pe args) ,(pe body))]
    [(expr-var type sym orig)
     orig]
    [(expr-arr type index size body)
     `(array ,(pe index) ,(pe size) ,(pe body))]
    [(expr-sum type index start end body)
     `(summate (,(pe index) ,(pe start) ,(pe end)) ,(pe body))]
    [(expr-prd type index start end body)
     `(product (,(pe index) ,(pe start) ,(pe end)) ,(pe body))]
    [(expr-if type tst thn els)
     `(if ,(pe tst) ,(pe thn) ,(pe els))]
    [(expr-app type rator rands)
     `(,(pe rator) ,@(map pe rands))]
    [(expr-let type var val body)
     `(let (,(pe var) ,(pe val)) ,(pe body))]
    [(expr-intr s)
     s]
    [(expr-val t v)
     v]
    [else e]))

(struct expr-fun  (args ret-type body)
  #:methods gen:custom-write
  [(define write-proc
     (lambda (fn port mode) (fprintf port "~a" (print-expr fn))))])
(struct expr-let  (type var val body))
(struct expr-var  (type sym orig)
  #:methods gen:custom-write
  [(define write-proc
     (lambda (v port mode) (fprintf port "~a" (expr-var-sym v))))])
(struct expr-arr  (type index size body))
(struct expr-sum  (type index start end body))
(struct expr-prd  (type index start end body))
(struct expr-if   (type tst thn els))
(struct expr-app  (type rator rands))
(struct expr-val  (type v))
(struct expr-intr (sym))

(define internal-ops
  (apply set '(size index recip nat2prob prob2real + * == and < or not)))
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

(define (simplify-expr expr)
  (define (sa e env)
    (match e
      [`(fn ,args ,ret-type ,body)
       (define aes (for/list [(arg args)]
                    (expr-var (cadr arg) (car arg) (car arg))))
       (expr-fun aes ret-type
                 (sa body (for/fold [(env env)] [(arg args)
                                                 (ae aes)]
                            (hash-set env (car arg) ae))))]
      [`((let (,var ,val ,type) ,body) : ,t)
       (define ve (expr-var type var var))
       (expr-let t ve (sa val env) (sa body (hash-set env var ve)))]
      [`((summate (,index ,start ,end) ,body) : ,type)
       (define ie (expr-var 'nat (gensym^ 'si) index))
       (expr-sum type ie (sa start env) (sa end env)
                 (sa body (hash-set env index ie)))]
      [`((product (,index ,start ,end) ,body) : ,type)
       (define ie (expr-var 'nat (gensym^ 'pi) index))
       (expr-prd type ie (sa start env) (sa end env)
                 (sa body (hash-set env index ie)))]
      [`((array (,index ,size) ,body) : ,type)
       (define ie (expr-var 'nat (gensym^ 'ai) index))
       (expr-arr type  ie (sa size env) (sa body (hash-set env index ie)))]
      [`((match ,tst (true ,thn) (false ,els)) : ,type)
       (expr-if type (sa tst env) (sa thn env) (sa els env))]
      [`((,rator ,rands ...) : ,type)
       (expr-app type (sa rator env) (map (curryr sa env) rands))]
      [`(,s : ,type) #:when (symbol? s)
       (hash-ref env s)]
      [(? symbol?) #:when (set-member? internal-ops e)
       (expr-intr e)]
      [`(,s : ,type) #:when (number? s)
       (expr-val type s)]))
  (sa expr (make-immutable-hash)))

(define (find-free-vars expr)
  (match expr
    [(expr-sum t i start end b)
     (define fbs (find-free-vars b))
     (set-union(find-free-vars start)
               (find-free-vars end)
               (set-remove
               (find-free-vars b)
                i))]
    [(expr-let t var val b)
     (set-union
      (find-free-vars val)
      (set-remove (find-free-vars b)
                  var))]
    [(expr-prd t i start end b)
     (set-union (find-free-vars start)
                (find-free-vars end)
                (set-remove
                 (find-free-vars b)
                 i))]
    [(expr-arr t i end b)
     (set-union(find-free-vars end)
               (set-remove
                (find-free-vars b)
                i))]
    [(expr-if t tst thn els)
     (set-union (find-free-vars tst)
                (find-free-vars thn)
                (find-free-vars els))]
    [(expr-app t rator rands)
     (apply set-union (map find-free-vars rands))]
    [(expr-val t v)
     (seteqv)]
    [(expr-intr sym)
     (seteqv)]
    [(expr-var t s o)
     (seteqv expr)]))

(define (flatten-expr expr)
  (define fns (box '()))
  (define (add-to-funs fn-name fn-expr)
    (set-box! fns (cons (cons fn-name fn-expr) (unbox fns))))
  
  (define (uf body)
    (match body
      [(expr-sum t i start end b)
       (define free-vars (set->list (find-free-vars body)))
       (define fn (expr-fun free-vars t (expr-sum t i start (uf end) (uf b))))
       (define fn-name (gensym^ 'fn))
       (add-to-funs fn-name fn)
       (expr-app t fn-name free-vars)]
      [(expr-let type var val b)
       (expr-let type var (uf val) (uf b))]
      [(expr-prd t i start end b)
       (define free-vars (set->list (find-free-vars body)))
       (define fn (expr-fun free-vars t (expr-prd t i start (uf end) (uf b))))
       (define fn-name (gensym^ 'fn))
       (add-to-funs fn-name fn)
       (expr-app t fn-name free-vars)]
      [(expr-arr t i end b)
       (define free-vars (set->list (find-free-vars body)))
       (define fn (expr-fun free-vars t (expr-arr t i (uf end) (uf b))))
       (define fn-name (gensym^ 'fn))
       (add-to-funs fn-name fn)
       (expr-app t fn-name free-vars)]
      [(expr-if t tst thn els)
       (expr-if t (uf tst) (uf thn) (uf els))]
      [(expr-app t rt rds)
       (expr-app t rt (map uf rds))]
      [else body]))
  (match expr
    [(expr-fun args ret-type body)
     (add-to-funs 'main (expr-fun args ret-type (uf body)))
     (unbox fns)]))

(define (get-rator-sym rator-sym rands)
  rator-sym)
(define (get-value v type)
  (match type
    ['nat `(#%ui-value ,v nat)]
    ['prob `(#%fl-value ,v prob)]))
(define (expand-spa fnps)
  (define (ef b)
    (match b
      [(expr-app t rt rds)
       `(#%app ,(get-rator-sym (ef rt) rds) ,@(map ef rds))]
      [(expr-var t sym o)
       sym]
      [(expr-intr sym)
       sym]
      [(expr-val t v)
       (get-value v t)]
      [else b]))
  (define (expand-fnb b to)
    (match b
      [(expr-sum t i start end b)
       (define tmp (gensym^ 'st))
       (define tmpi (gensym^ 'sti))
       `(let1 (,tmp : ,t ,(get-value 0 t))
              (block
               (for1 ((,(ef i) ,(ef start) (#%app jit-add-nuw ,(ef i) (#%ui-value 1 nat))
                       : ,(expr-var-type i))
                      (#%app jit-icmp-ult ,(ef i) ,(ef end)))
                     (let1 (,tmpi : ,t ,(get-value 0 t))
                           (block
                            ,(expand-fnb b tmpi)
                            (set! ,tmp (#%app jit-add-nuw ,tmp ,tmpi)))))
               (set! ,to ,tmp)))]
      [(expr-let type var val b)
       `(let1 (,(ef var) : ,(expr-var-type var) ,(ef val))
              ,(expand-fnb b to))]
      [(expr-prd t i start end b)
       (define tmp (gensym^ 'pt))
       (define tmpi (gensym^ 'pti))
       `(let1 (,tmp : ,t (#%ui-value 1 nat))
              (for1 ((,(ef i) ,(ef start) (#%app jit-add-nuw ,(ef i) (#%ui-value 1 nat))
                      : ,(expr-var-type i))
                 (< ,(ef i) ,(ef end)))
                ,(expand-fnb b tmpi)
                (set! ,tmp (* ,tmp ,tmpi)))
          (set! ,to ,tmp))]
      [(expr-arr t i end b)
       (define tmp (gensym^ 'at))
       (define tmpi (gensym^ 'ati))
       `(let1 (,tmp : ,t (,(symbol-append 'empty-array- (cadr t)) ,(ef end)))
              (for1 ((,(ef i)
                      (#%ui-value 0 nat)
                      (#%app jit-add-nuw ,(ef i) (#%ui-value 1 nat))
                      : ,(expr-var-type i))
                 (#%app jit-icmp-ult ,(ef i) ,(ef end)))
                ,(expand-fnb b tmpi)
                (,(symbol-append (symbol-append 'set-array- (cadr t)) '-at-index!)
                 ,(ef i) ,tmpi))
          (set! ,to ,tmp))]
      [(expr-if t tst thn els)
       `(if ,(ef tst)
            ,(expand-fnb thn to)
            ,(expand-fnb els to))]
      [(expr-app t rt rds)
       `(set! ,to ,(ef b))]
      [(expr-val t v)
       `(set! ,to ,v)]))
  (for/list ([fnp fnps])
    (define fn-name (car fnp))
    (define fn (cdr fnp))
    (match fn
      [(expr-fun args ret-type b)
       `(function ,fn-name ,(map (lambda (arg)
                                   `(,(ef arg) : ,(get-type (expr-var-type arg))))
                                 args)
                  : ,ret-type ,(expand-fnb b 'ret))])))

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
      [`(let ((,args : ,tys ,vals) ...) ,body)
       `(let ,(for/list ([arg args]
                         [ty tys]
                         [v vals])
                `(,arg : ,(get-type ty) ,v))
          ,(cs body arg-types))]
      [`(block ,exps ...)
       `(block ,@(map (curryr cs arg-types) exps))]
      [`(for1 ,lv ((,i ,start ,next : ,ty) ,tst) ,body)
       `(let ((,i : ,ty ,(ce start arg-types)))
          (block
           (while (,@lv (,i : ,ty)) ,(ce tst arg-types)
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
       `(#%exp (#%app set-array-prob-at-index! ,arr
                      ,(ce i arg-types) ,(ce v arg-types)))]
      [`(return ,v)
       `(return ,v)]
      [else (error "unknown expression in compile-to-jit" body)]))
  (match body
    [`(fn ,args ,ret-type ,body)
     (define arg-types (for/hash ([arg args])
                         (values (car arg) (cadr arg))))
     `(#%module
          ,@(basic-defines)
          (define-function (f ,@(for/list ([arg args])
                                `(,(car arg) : ,(get-type (cadr arg))))
                            : ,(get-type ret-type))
            ,(cs body arg-types)))]))

(define compilers (list reduce-function
                        simplify-expr
                        flatten-expr
                        expand-spa
                        ;; reduce-to-folds
                        ;; reduce-folds
                        ;; compile-to-jit-lang
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
  (error 'stop)
  (compile-module prog-ast))

(module+ test
  (require ffi/unsafe)

  (require "../disassemble/disassemble/main.rkt")
  ;; (disassemble-ffi-function fun #:size 100)

  (define hello-src (read-file "examples/hello-full.hkr"))
  (define nbg-src (read-file "examples/naive-bayes-gibbs-full.hkr"))
  (define nbgo-src (read-file "examples/naive-bayes-gibbs-opt.hkr"))
  ;; (debug-program hello-src compilers)
  ;; (define hello-env (debug-program hello-src compilers))
  ;; (define (get-hello-f f)
  ;;   (jit-get-function (env-lookup f hello-env)))
  ;; (define test-array ((get-hello-f 'make-array-real)  4
  ;;                     (list->cblock `(80.0 20.1 30.2 40.4 ) real-type)))


  ;; (printf "test value: ~a\n"((get-hello-f 'f) test-array))

  (debug-program nbgo-src compilers)
  (define nbg-env (debug-program nbg-src compilers))
  ;; (jit-dump-module   nbg-env)
  (define (get-nbg-f f)
    (jit-get-function (env-lookup f nbg-env)))
  (define prob-type (jit-get-racket-type (env-lookup 'prob nbg-env)))
  (define nat-type (jit-get-racket-type (env-lookup 'nat nbg-env)))

  (define make-array-prob (get-nbg-f 'make-array-prob))
  (define make-array-nat (get-nbg-f 'make-array-nat))

  (define topic-prior
    (make-array-prob 100
                     (list->cblock '(0.014228 0.003821 0.030999 0.002363
                                              0.024379 0.001317 0.002707
                                              0.013426 0.000219 0.008970
                                              0.012040 0.003862 0.006986
                                              0.012392 0.004347 0.003219
                                              0.011661 0.006158 0.011185
                                              0.060091 0.008681 0.000573
                                              0.019064 0.010391 0.005424
                                              0.022017 0.015388 0.001420
                                              0.023657 0.007121 0.003429
                                              0.004026 0.005844 0.003225
                                              0.045181 0.006403 0.018045
                                              0.006239 0.008657 0.000347
                                              0.002385 0.000023 0.005600
                                              0.032088 0.005185 0.014991
                                              0.003511 0.001131 0.004627
                                              0.002341 0.013252 0.016136
                                              0.017101 0.003137 0.003582
                                              0.000282 0.000733 0.018463
                                              0.004997 0.000185 0.029280
                                              0.005629 0.018106 0.016644
                                              0.013107 0.002684 0.019586
                                              0.004801 0.020799 0.008960
                                              0.013165 0.000553 0.000053
                                              0.001443 0.000540 0.008137
                                              0.005964 0.004829 0.005559
                                              0.004122 0.000518 0.018653
                                              0.022602 0.007208 0.007697
                                              0.037427 0.002005 0.002432
                                              0.012954 0.000968 0.001409
                                              0.005069 0.019474 0.015708
                                              0.001842 0.009659 0.009836
                                              0.010813 0.029945 0.000568)
                                   prob-type)))
  (define word-prior
    (make-array-prob 3 (list->cblock '(0.023754 0.277952 0.698294) prob-type)))
  (define z
    (make-array-nat 20 (list->cblock '(1 0 2 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 )
                                     nat-type)))
  (define w
    (make-array-nat 120 (list->cblock '(40 0 14 0 55 0 68 0 50 0 67 0 44 0 31 0
                                           89 0 66 0 29 0 43 0 57 0 69 0 75 0 77
                                           0 44 0 39 0 40 0 97 0 53 0 96 0 43 0
                                           26 0 36 0 8 0 78 0 94 0 82 0 8 0 77 0
                                           55 0 7 0 86 0 42 0 95 0 10 0 94 0 22
                                           0 73 0 30 0 88 0 14 0 90 0 18 0 47 0
                                           52 0 26 0 30 0 76 0 74 0 92 0 20 0 63
                                           0 50 0 74 0 51 0 81 0 76 0 60 0 )
                                      nat-type)))
  (define doc (make-array-nat 120 (list->cblock '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                                                    0 0 0 0 0 0 0 0 0 0 0 0 0 0
                                                    0 0 0 0 0 0 0 0 0 0 0 1 0 1
                                                    0 1 0 1 0 1 0 1 0 1 0 1 0 1
                                                    0 1 0 1 0 1 0 1 0 1 0 1 0 1
                                                    0 1 0 1 0 1 0 1 0 2 0 2 0 2
                                                    0 2 0 2 0 2 0 2 0 2 0 2 0 2
                                                    0 2 0 2 0 2 0 2 0 2 0 2 0 2
                                                    0 2 0 2 0 2 0 )
                                                nat-type)))
  (define f (get-nbg-f 'f))
  (define result_raw (time (f topic-prior word-prior z w doc 1)))
  (define get-array-prob (get-nbg-f 'get-array-prob))
  (define result (cblock->list (get-array-prob result_raw) prob-type 120))
  (pretty-display result)
  )
