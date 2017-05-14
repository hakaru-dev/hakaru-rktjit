#lang racket

(require "ast.rkt")
(require "utils.rkt")

(provide expand-lc)

(define (get-type tast)
  (match tast
    [`(array ,t)
     (symbol-append (symbol-append 'array- t) '-p)]
    [else tast]))
(define op-map (make-hash '((+ . jit-add-nuw)
                            (* . jit-mul-nuw)
                            (< . jit-icmp-ult)
                            (== . jit-icmp-eq)
                            (and . jit-and)
                            (or . jit-or)
                            (recip . recip-real)
                            (not . jit-not)
                            )))
(define (expr-type e)
  (match e
    [(expr-app t _ _)
     t]
    [(expr-var t _ _)
     t]
    [(expr-val t _)
     t]))
(define (get-rator-sym rator-sym rands)
  (match rator-sym
    ['index
     (match (cadr (expr-var-type (car rands)))
       ['nat 'index-array-nat]
       ['prob 'index-array-prob])]
    ['size
     (match (cadr (expr-var-type (car rands)))
       ['nat 'size-array-nat]
       ['prob 'size-array-prob])]
    ['+
     (match (expr-type (car rands))
       ['nat 'jit-add-nuw]
       ['prob 'jit-fadd])]
    ['*
     (match (expr-type (car rands))
       ['nat 'jit-mul-nuw]
       ['prob 'jit-fmul])]
    [else (hash-ref op-map rator-sym rator-sym)]))
(define (get-value v type)
  (match type
    ['nat `(#%ui-value ,v nat)]
    ['prob `(#%fl-value ,(exact->inexact v) prob)]))

(define (expand-lc fnps)
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
       `(let ((,tmp : ,t ,(get-value 0 t))
              (,(ef i) : ,(expr-var-type i) ,(ef start)))
          (block
           (while ((,(ef i) : ,(expr-var-type i)) (,tmp : ,(get-type t)))
             (#%app jit-icmp-ult ,(ef i) ,(ef end))
             (let ((,tmpi : ,t ,(get-value 0 t)))
               (block
                ,(expand-fnb b tmpi)
                (set! ,tmp (#%app ,(if (eq? t 'nat)
                                       'jit-add-nuw
                                       'jit-fadd)
                                  ,tmp ,tmpi))
                (set! ,(ef i) (#%app jit-add-nuw ,(ef i) (#%ui-value 1 nat))))))
           (set! ,to ,tmp)))]
      [(expr-let type var val b)
       `(let ((,(ef var) : ,(expr-var-type var) ,(ef val)))
              ,(expand-fnb b to))]
      [(expr-prd t i start end b)
       (define tmp (gensym^ 'pt))
       (define tmpi (gensym^ 'pti))
       `(let ((,tmp : ,t ,(get-value 0 t))
              (,(ef i) : ,(expr-var-type i) ,(ef start)))
          (block
           (while ((,(ef i) : ,(expr-var-type i)) (,tmp : ,(get-type t)))
             (#%app jit-icmp-ult ,(ef i) ,(ef end))
             (let ((,tmpi : ,t ,(get-value 0 t)))
               (block
                ,(expand-fnb b tmpi)
                (set! ,tmp (#%app ,(if (eq? t 'nat)
                                       'jit-mul-nuw
                                       'jit-fmul)
                                  ,tmp ,tmpi))
                (set! ,(ef i) (#%app jit-add-nuw ,(ef i) (#%ui-value 1 nat))))))
           (set! ,to ,tmp)))]
      [(expr-arr t i end b)
       (define tmp (gensym^ 'at))
       (define tmpi (gensym^ 'ati))
       `(let ((,tmp : ,(get-type t)
                    (#%app ,(symbol-append 'empty-array- (cadr t)) ,(ef end)))
              (,(ef i) : ,(expr-var-type i) (#%ui-value 0 nat)))
          (block
           (while ((,(ef i) : ,(expr-var-type i)) (,tmp : ,(get-type t)))
             (#%app jit-icmp-ult ,(ef i) ,(ef end))
             (let ((,tmpi : ,(cadr t) ,(get-value 0 (cadr t))))
                   (block
                    ,(expand-fnb b tmpi)
                    (#%exp (#%app
                      ,(symbol-append (symbol-append 'set-array- (cadr t)) '-at-index)
                      ,tmp ,(ef i) ,tmpi))
                    (set! ,(ef i) (#%app jit-add-nuw ,(ef i) (#%ui-value 1 nat))))))
           (set! ,to ,tmp)))]
      [(expr-if t tst thn els)
       `(if ,(ef tst)
            ,(expand-fnb thn to)
            ,(expand-fnb els to))]
      [(expr-app t rt rds)
       `(set! ,to ,(ef b))]
      [(expr-val t v)
       `(set! ,to ,(ef b))]))
  (for/list ([fnp fnps])
    (define fn-name (car fnp))
    (define fn (cdr fnp))
    (match fn
      [(expr-fun args ret-type b)
       `(define-function ,@(if (eq? fn-name 'main)
                               '()
                               `((#:attr AlwaysInline)))
          (,fn-name ,@(map (lambda (arg)
                             `(,(ef arg) : ,(get-type (expr-var-type arg))))
                           args)
                    : ,(get-type ret-type))
          (let ((ret : ,(get-type ret-type) (#%ui-value 0 int)))
            (block
             ,(expand-fnb b 'ret)
             (return ret))))])))
