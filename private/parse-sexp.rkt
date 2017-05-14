#lang racket


(require "ast.rkt")
(require "utils.rkt")

(provide parse-sexp)

(define (sa e env)
  (match e
    [`(fn ,args ,ret-type ,body)
     (define aes (for/list [(arg args)]
                   (expr-var (cadr arg) (car arg) (car arg))))
     (define arg-env
       (for/fold [(env env)]
                 [(arg args)
                  (ae aes)]
         (hash-set env (car arg) ae)))
     (expr-fun aes ret-type
               (sa body arg-env))]
    [`((let (,var ,val ,type) ,body) : ,t)
     (define ve (expr-var type var var))
     (define vale (sa val env))
     (expr-let t ve vale (sa body (hash-set env var ve)))]
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
     (define randse (map (curryr sa env) rands))
     (expr-app type (sa rator env) randse)]
    [`(,s : ,type) #:when (symbol? s)
     (hash-ref env s)]
    [(? symbol?) #:when (set-member? internal-ops e)
     (expr-intr e)]
    [`(,s : ,type) #:when (number? s)
     (expr-val type s)]))

;;S-expression to ast struct
(define (parse-sexp expr)
  (sa expr (make-immutable-hash)))

(module+ test
  (define ps
    (parse-sexp
     '(fn
          ((topic_prior (array prob))
           (word_prior (array prob))
           (z (array nat))
           (w (array nat))
           (doc (array nat))
           (docUpdate nat))
        (array prob)
        ((let (topic_prior_size ((size (topic_prior : (array prob))) : nat) nat)
           (topic_prior : (array prob)))
         :
         (array prob))))))

