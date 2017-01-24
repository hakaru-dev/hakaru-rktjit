;; (define (get-op op)
;;   (match op
;;     ['+ 'jit-add]
;;     ['* 'jit-mul]
;;     ['== 'jit-eq?]
;;     ['&& 'jit-and]
;;     ['< 'jit-lt?]
;;     ['|| 'jit-or]
;;     [x #:when (set-member? (set 'recip 'prob2real 'nat2prob 'size 'index) op)
;;      op]
;;     [else (error "unknown operator" op)]))

;; (define (env-extend sym val env)
;;   (cons (cons sym val) env))
;; (define (lookup-env sym env)
;;   (cdr (assoc sym env)))
;; (define (compile-rjit-exp body assign-to type)
;;   (match body
;;     [`(summate ,index from ,start to ,end : ,body)

;;      (define sum-result (gensym^ 'sr))
;;      (define sum-current (gensym^ 'sc))
;;      (define sum-iter (gensym^ 'si))
;;      (define init-iter (gensym^ 'ii))
;;      (define end-iter (gensym^ 'se))
;;      `(define-variables ((,sum-result type)
;;                          (,end-iter nat)
;;                          (,init-iter nat))
;;         (block
;;          ,(compile-rjit-exp start init-iter 'nat)
;;          ,(compile-rjit-exp end end-iter 'nat)
;;          (assign ,sum-result ,(vlb 0 'real))
;;          ,(for-loopb sum-iter
;;                      'nat
;;                      init-iter
;;                      (curryr appb-lt? end-iter)
;;                      (curryr appb-add (vi 1))
;;                      (λ (i) (combine-blocks
;;                              `(block
;;                                ,(compile-rjit-exp body 
;;                                                   sum-current type)
;;                                (assign ,sum-result
;;                                        ,(appb-add sum-current
;;                                                   sum-result))))))
;;          (assign ,assign-to ,sum-result)))]
;;     [`(array ,index of ,size : ,body)
;;      `(array ,index of ,size :
;;              ,(compile-rjit-exp body 'arry 'real))]
;;     [`(product ,index from , start to ,end : ,body)
;;      (define prod-result (gensym^ 'pr))
;;      (define prod-current (gensym^ 'pc))
;;      (define prod-iter (gensym^ 'pi))
;;      (define init-iter (gensym^ 'ii))
;;      (define end-iter (gensym^ 'se))
;;      `(define-variables ((,prod-result type)
;;                          (,prod-iter nat)
;;                          (,init-iter nat))
;;         (block
;;          ,(compile-rjit-exp start init-iter 'nat)
;;          ,(compile-rjit-exp end end-iter 'nat)
;;          (assign ,prod-result ,(vlb 1 'real))
;;          ,(for-loopb prod-iter
;;                      'nat
;;                      init-iter
;;                      (curryr appb-lt? end-iter)
;;                      (curryr appb-add (vi 1))
;;                      (λ (i) (combine-blocks
;;                              `(block
;;                                ,(compile-rjit-exp body 
;;                                                   prod-current type)
;;                                (assign ,prod-result
;;                                        ,(appb-mul prod-current
;;                                                    prod-result))))))
;;          (assign ,assign-to ,prod-result)))]
;;     [`(match ,tst : true: ,thn false: ,els)
;;      (define tst-sym (gensym^ 'mc))
;;      (define tst-exp (compile-rjit-exp tst tst-sym 'nat))
;;      `(if ,tst-sym
;;           ,(compile-rjit-exp thn assign-to type)
;;           ,(compile-rjit-exp els assign-to type))]
;;     [(? number?)
;;      `(assign ,assign-to ,(vlb body 'nat))]
;;     [(? symbol?)
;;      `(assign ,assign-to ,body)]
;;     [`(,op ,rands ...)
;;      (define rand-syms (map (λ (x) (gensym^ 'r)) rands))
;;      (define rand-exps (map (λ (e s) 
;;                                (compile-rjit-exp e s 'real))
;;                             rands rand-syms))
;;      (combine-blocks
;;       `(block
;;         ,@rand-exps
;;         (assign ,assign-to (#%app ,(get-op op) ,@rand-syms))))]))
; (define (add-basic-types)
;;   `((define-function (prob2real (x : real) : real)
;;       (return x))
;;     (define-function (nat2real (x : nat) : real)
;;       (return (cast x nat real))) ;;todo add cast to jit
;;     (define (recip (x : real) : real)
;;       (return (#%app jit-div ,(vi 1) x)))))

;; (define (compile-hakaru prg)
;;   (define (compile-type type)
;;     (match type
;;       [`(array ,t)
;;        (symbol-append 'array- t)]
;;       [else type]))
;;   (define (compile-arg arg)
;;     (match arg
;;       [`(,sym . ,type)
;;        `(,sym : ,(compile-type type))]))
;;   (define (compile-exp exp var-types)
;;     (define return-result (gensym^ 'result))
;;     `(define-variable (,return-result array-prob)
;;        (block
;; 	,(compile-rjit-exp exp return-result 'real)
;; 	(return ,return-result))))
;;   (define-values (var-types body) (combine-functions prg))
;;   `(module
;;        ,@(add-basic-types)
;;        (define-function (program ,@(map compile-arg var-types) : array-prob
;;                                  ,(compile-exp body var-types)))))
