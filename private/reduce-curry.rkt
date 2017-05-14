#lang racket

(provide reduce-curry)

(define (reduce-curry expr)
  (define (combine-functions prg [var-type-assoc null])
    (match prg
      [`((fn ,var ,type ,body) : ,t)
       (combine-functions body (cons (list var (car t)) var-type-assoc))]
      [else (values (reverse var-type-assoc) prg)]))
  (define-values (args fbody) (combine-functions expr))
  (match fbody
    [`(,body : ,type)
     `(fn ,args ,type ,fbody)]))
