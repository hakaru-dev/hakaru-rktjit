#lang racket
(require "ast.rkt"
         "utils.rkt")

(provide debug-curry
         clean-curry)

(define debug-curry (make-parameter #f))
(define dp (debug-printf debug-curry))

(define (remove-curry src args (args-info '()))
  (match src
    [`((fn ,var ,ret-type ,body) : ,fn-type)
     (define arg-info (assoc var args-info))
     (define var-type (get-type-with-info (car fn-type) arg-info))
     (dp "arg: ~a, info: ~a, type: ~a\n" var arg-info var-type)
     (define evar (expr-var var-type var arg-info))
     (remove-curry body (append args (list evar)) args-info)]
    [`(,body : ,t)
     (expr-fun 'prog args t src)]))


(define (clean-curry st)
  (match st
    [(state src info passes)
     (define new-src (remove-curry src '() (hash-ref info 'arg-info)))
     (dp "debug-curry: out \n~a\n" (pe new-src))
     (run-next new-src info st)]))
