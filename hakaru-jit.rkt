#lang racket

(require "../libjit/jit.rkt")
(require "../libjit/jit-utils.rkt")

(define (read-file filename)
  (call-with-input-file filename
    (lambda (in)
      (read in))))

(define internal-ops
  (apply set '(size index recip nat2prob prob2real + * == && < ||)))
(define internal-loop-ops
  (set 'summate 'product 'array))
;;LEVEL1
(define (reduce-function exp)
  (define (combine-functions prg [var-type-assoc null])
    (match prg
      [`(function : ,var ,type : ,body)
       (combine-functions body (cons (cons var type) var-type-assoc))]
      [else (values (reverse var-type-assoc) prg)]))
  (define-values (args fbody) (combine-functions exp))
  `(function (,@args) ,fbody))

;;LEVEL2
(define (simplify-exp exp)
  (define sa simplify-exp)
  (match exp
    [`(function ,args ,body)
     `(function ,args ,(sa body))]
    [`(,op ,index from ,start to ,end : ,body)
     `(,op (,index ,(sa start) ,(sa end)) ,(sa body))]
    [`(array ,index of ,size : ,body)
     `(array (,index 0 ,(sa size)) ,(sa body))]
    [`(match ,tst : true: ,thn false: ,els)
     `(if ,(sa tst) ,(sa thn) ,(sa els))]
    [`(,rator ,rands ...)
     `(,rator ,@(map sa rands))]
    [else exp]))

;;LEVEL3
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
      [`(function ,args ,body)
       `(function ,args
                  ,(u body
                      (map (λ (at) (cons (car at) (car at))) args)))]
      [`(,op (,index ,start ,end) ,body) #:when (set-member? internal-loop-ops op)
       (define i (gensym^ index))
       (define nv (cons (cons index i) vars))
       `(,op (,i ,(u start vars) ,(u end vars)) ,(u body nv))]
      [`(if ,tst ,thn ,els)
       `(if ,(u tst vars) ,(u thn vars) ,(u els vars))]
      [`(,rator ,rands ...)
       `(,rator ,@(map (curryr u vars) rands))]
      [else (rv exp vars)]))
  (u exp '()))

;;LEVEL4
(define (simplify-end-iter exp)
  (define sl simplify-end-iter)
  (match exp
    [`(function ,args ,body)
     `(function ,args ,(sl body))]
    [`(,op (,index ,start ,end) ,body)
     #:when (set-member? internal-loop-ops op)
     (if (or (symbol? end) (number? end) (equal? (car end) 'size))
         `(,op (,index ,start ,end) ,(sl body))
         (let ((end^ (gensym^ 'e)))
           `(let ((,end^ ,(sl end)))
              (,op (,index ,start ,end^) ,(sl body)))))]
    [`(,rator ,rands ...)
     `(,rator ,@(map sl rands))]
    [else exp]))

(define (reduce-folds body)
  (define (simple? x)
    (or (symbol? x) (number? x)))
  (define (wrap-simplify body)
    (if (simple? body)
        (values body identity)
        (let ((sym (gensym^ 's)))
          (values sym
                  (λ (prg) `(let (,sym ,body) ,prg))))))
  (define rh reduce-folds)
  (define (init-value op size)
    (match op
      ['summate 0]
      ['product 1]
      ['array `(new array real ,size)]
      [else (error "unknown op for init-value")]))
  (define (get-assign op result index body)
    (match op
      ['summate `(assign (+ ,result ,body))]
      ['product `(assign (* ,result ,body))]
      ['array `(assign (index ,result ,index) ,body)]))
  (match body
    [`(,op (,index ,start ,end) ,body) #:when (set-member? internal-loop-ops op)
     (define result (gensym^ 'r))
     `(fold-loop (,index ,start ,end)
                 (,result ,(init-value op end))
                 ,(get-assign op result index (rh body)))]
    [`(if ,tst ,thn ,els)
     `(if ,(rh tst) ,(rh thn) ,(rh els))]
    [`(function ,args ,body)
     `(function ,args ,(rh body))]
    [`(,rands ...)
     `(,@(map rh rands))]
    [else body]))


(define compilers (list reduce-function simplify-exp uniquify simplify-end-iter reduce-folds))
(define (debug-program prg cmplrs)
  (pretty-display
   (for/fold ([prg prg])
             ([c cmplrs])
     (pretty-display prg)
     (printf "applying ~a\n" (object-name c))
     (c prg))))


(module+ test
  (define hello-src (read-file "examples/hello.hkr"))
  (define nbg-src (read-file "examples/naive-bayes-gibbs.hkr"))
  (debug-program hello-src compilers)
  (debug-program nbg-src compilers)
  ;; (pretty-display (reduce-folds (simplify-end-iter (uniquify (simplify-exp (reduce-function hello-src))))))
  ;; (pretty-display (reduce-folds (simplify-end-iter (uniquify (simplify-exp (reduce-function nbg-src))))))
)

