#lang racket
(require "ast.rkt"
         "utils.rkt")

(provide debug-curry
         clean-curry)

(define debug-curry (make-parameter #f))
(define dp (debug-printf debug-curry))

(define (curry-at-stops src pos args args-info)
  (match src
    [`((fn ,var ,ret-type ,body) : ,fn-type)
     (define pad (build-string (add1 pos) (const #\_)))
     (define arg-info (vector-ref args-info pos))
     (define var-type (get-type-with-info (car fn-type) arg-info))
     (dp "debug-curry: ~a ~a : ~a\n"  pad var var-type)
     (define evar (expr-var var-type
                            var (build-var-info arg-info)))
     (if (member 'curry arg-info)
         (begin (dp "debug-curry: ~a **curried-here**\n" pad)
                (expr-fun (symbol-append 'prog (add1 pos))
                          (append args (list evar))
                          ret-type
                          (curry-at-stops body (+ pos 1) '() args-info)))
         (curry-at-stops body (add1 pos) (append args (list evar)) args-info))]
    [`(,body : ,t)
     (expr-fun (symbol-append 'prog pos) args t src)]))

(define (clean-curry st)
  (match st
    [(state src info passes)
     (define pai (hash-ref info prog-arg-info))
     (define new-src (curry-at-stops src 0 '() pai))
     (dp "debug-curry: out \n~a\n" (pe new-src))
     (run-next new-src info st)]))



(define (get-type-with-info var-type var-info)
  (define (number-type)
    (define infosym (symbol-append var-type 'info))
    (define type-info (assocv infosym var-info))
    (if type-info
        (let ([constant (assocv 'constant type-info)]
              [valuerange (assocv 'valuerange type-info)])
          (cond
            [constant `(,var-type (constant . ,constant))]
            [valuerange `(,var-type (valuerange . ,valuerange))]))
        var-type))
  (match var-type
    ['nat  (number-type)]
    ['real (number-type)]
    ['prob (number-type)]
    [`(pair ,at ,bt)
     (define pair-info (assocv 'pairinfo var-info))
     (if pair-info
         (let ([ainfo (assocv 'ainfo pair-info)]
               [binfo (assocv 'binfo pair-info)])
           `(pair ,(get-type-with-info at ainfo) ,(get-type-with-info bt binfo)))
         `(pair ,at ,bt))]
    [`(array ,type)
     (define array-info (assocv 'arrayinfo var-info))
     (if array-info
         (let ([size (assocv 'size array-info)]
               [typeinfo (assocv 'typeinfo array-info)])
           `(array ,(get-type-with-info type typeinfo) (size . ,size)))
         `(array ,type))]))
