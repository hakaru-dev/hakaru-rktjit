#lang racket

(require "ast.rkt")

(provide simplify)

(splicing-let ([gensym-hash (make-hash)])
  (define (gensym^ sym [sep ""])
    (define n (add1 (hash-ref gensym-hash sym 0)))
    (hash-set! gensym-hash sym n)
    (string->symbol (string-append (symbol->string sym)
                                   sep
                                   (number->string n)))))

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

(define (simplify expr)
  (sa expr (make-immutable-hash)))
