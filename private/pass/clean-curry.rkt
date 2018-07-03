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
     (dp "\n\ndoing fn for var: ~a\n" var)
     (define pad (build-string (add1 pos) (const #\_)))

     (unless (pair? args-info)
       (printf "no arg-info for function argument: ~a\nargs-info: ~a\n" var args-info)
       (error "arg-info not complete"))
     (define arg-info (car args-info))
     (dp "arg-info : ~a\n" arg-info)

     (define var-type (get-type-with-info (car fn-type) arg-info))
     (dp "var-type: ~a\n" var-type)
     (define evar (expr-var var-type var (build-var-info arg-info)))

     (define fninfo (assocv 'fninfo arg-info '()))
     (define remove? (member 'remove fninfo))
     (define movedown? (member 'movedown fninfo))
     (define curry? (member 'curry fninfo))

     (cond
       [remove?
        (define-values (nb neinfo)
          (curry-at-stops body (add1 pos) args (cdr args-info)))
        (define ninfo (hash-set neinfo var arg-info))
        (values nb (hash-set ninfo 'removed-vars (cons var (hash-ref ninfo 'removed-vars '()))))]

       [movedown?
        (match-define `(,curr-type -> ,curr-rst) fn-type)
        (dp "movedown?: t: ~a\n" fn-type)
        (match body
          [`((fn ,nvar ,nret-type ,nbody) : ,nfn-type)
           (match-define `(,next-type -> ,next-rest) nfn-type)
           (define new-curr-info (append (assocvr 'fninfo arg-info)
                                         `((fninfo . ,(remove 'movedown fninfo)))))
           (define new-info `(,(second args-info) ,new-curr-info ,@(cddr args-info)))
           (define b `((fn ,var ,ret-type ,nbody) : (,curr-type -> ,next-rest)))
           (curry-at-stops `((fn ,nvar ,nret-type ,b) : (,next-type -> (,curr-type -> ,next-rest)))
                           pos args new-info)])]

       [curry?
        (dp "debug-curry: ~a **curried-here**\n" pad)
        (define-values (nb ninfo)
          (curry-at-stops body (+ pos 1) '() (cdr args-info)))
        (values
         (expr-fun (symbol-append 'prog (add1 pos))
                   (append args (list evar)) ret-type nb)
         (hash-set ninfo var arg-info))]

       [else
        (define-values (nb ninfo)
          (curry-at-stops body (add1 pos) (append args (list evar)) (cdr args-info)))
        (values nb (hash-set ninfo var arg-info))])]
    [`(,body : ,t)
     (values (expr-fun (symbol-append 'prog pos) args t src) (make-immutable-hash))]))


(define (clean-curry st)
  (match st
    [(state src info passes)
     (define pai (hash-ref info prog-arg-info))
     (dp "orig-arg-info: ~a\n" pai)
     (define-values (new-src new-arg-info)
       (curry-at-stops src 0 '() pai))
     (dp "\n\nnew-arg-info: \n~a" (pretty-format new-arg-info))
     (dp "debug-curry: out \n~a\n" (pe new-src))
     (run-next new-src new-arg-info st)]))
