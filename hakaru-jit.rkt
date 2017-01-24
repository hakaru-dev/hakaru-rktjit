#lang racket

(require "../libjit/jit.rkt")
(require "../libjit/jit-utils.rkt")

(define (read-file filename)
  (call-with-input-file filename
    (lambda (in)
      (read in))))


(define (reduce-function exp)
  (define (combine-functions prg [var-type-assoc null])
    (match prg
      [`(function : ,var ,type : ,body)
       (combine-functions body (cons (cons var type) var-type-assoc))]
      [else (values (reverse var-type-assoc) prg)]))
  (define-values (args fbody) (combine-functions exp))
  `(function (,@args) ,fbody))

(define (simple? x)
  (or (symbol? x) (number? x)))

(define (reduce-folds body)
  (define (wrap-simplify body)
    (if (simple? body)
        (values body identity)
        (let ((sym (gensym^ 's)))
          (values sym
                  (λ (prg) `(let (,sym ,body) ,prg))))))
  (define rh reduce-folds)
  (match body
    [`(summate ,index from ,start to ,end : ,body)
     (define-values (start-exp sf) (wrap-simplify (rh start)))
     (define-values (end-exp ef) (wrap-simplify (rh end)))
     (define result (gensym^ 'r))
     (sf (ef
          `(let (,result 0)
             (begin
               (loop
                (,index ,start-exp ,end-exp)
                (assign ,result (+ ,result ,(rh body))))
               ,result))))]
    [`(product ,index from ,start to ,end : ,body)
     (define-values (start-exp sf) (wrap-simplify (rh start)))
     (define-values (end-exp ef) (wrap-simplify (rh end)))
     (define result (gensym^ 'r))
     (sf (ef
          `(let (,result 1)
             (begin
               (loop
                (,index ,start-exp ,end-exp)
                (assign ,result (* ,result ,(rh body))))
               ,result))))]
    [`(array ,index of ,size : ,body)
     (define-values (size-exp sf) (wrap-simplify (rh size)))
     (define result (gensym^ 'ar))
     (sf
      `(let (,result (new array 'real ,size-exp))
         (begin
           (loop (,index 0 ,size-exp)
                 (assign (index ,result ,index) ,(rh body)))
           ,result)))]
    [`(match ,tst : true: ,thn false: ,els)
     `(if ,(rh tst) ,(rh thn) ,(rh els))]
    [`(function ,args ,body)
     `(function ,args ,(rh body))]
    [`(,rands ...)
     `(,@(map rh rands))]
    [else body]))


(define internal-ops
  (apply set '(size index recip nat2prob prob2real + * == && < ||)))


(module+ test
  (define hello-src (read-file "examples/hello.hkr"))
  (define nbg-src (read-file "examples/naive-bayes-gibbs.hkr"))
  (display hello-src) (newline)
  (pretty-display (reduce-folds (reduce-function hello-src)))
  (pretty-display (reduce-folds (reduce-function nbg-src))))

