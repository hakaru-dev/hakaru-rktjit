#lang racket

(require racket/splicing
         "ast.rkt")

(provide (all-defined-out))

(define debug-pass (make-parameter #f))
(define-syntax-rule (dprintf tst args ...)
  (when (and (debug-pass) tst) (printf args ...)))

(define-syntax-rule (dtprintf args ...) (dprintf #t args ...))

(define (symbol-append s1 s2) (string->symbol (format "~a~a" s1 s2)))

(define (change-orig-var v o)
  (set-expr-var-info! v o))

(define csym '$c)
(define msym '$m)
(define (set-mutable-var v) (change-orig-var v msym) v)
(define (set-constant-var v) (change-orig-var v csym) v)

(define (is-mutable-var? v) (equal? (expr-var-info v) msym))
(define (is-constant-var? v) (equal? (expr-var-info v) csym))

(define (wrap-expr typs vars vals s b)
  (cond [(list? vars) (expr-lets typs vars vals s b)]
        [(expr-var? vars) (expr-lets (list typs) (list vars) (list vals) s b)]
        [else (error "wrap-expr-unknown-vars" typs vars (pe vals) (ps s) (pe b))]))

(define (var-sym-append v t sym (o '_))
  (expr-var t (symbol-append (expr-var-sym v) sym) o))

(define (get-type-with-info var-type var-info)
  (define (number-type)
    (define infosym (symbol-append var-type '-info))
    (define type-info (assocv infosym var-info))
    (if type-info
        (let ([constant (assocv 'constant type-info)]
              [valuerange (assocv 'value-range type-info)])
          (cond
            [constant `(,var-type (constant . ,constant))]
            [valuerange `(,var-type (value-range . ,valuerange))]))
        var-type))
  (match var-type
    ['nat  (number-type)]
    ['real (number-type)]
    ['prob (number-type)]
    ['bool (number-type)]
    [`(pair ,at ,bt)
     (define pair-info (assocv 'pair-info var-info))
     (if pair-info
         (let ([ainfo (assocv 'ainfo pair-info)]
               [binfo (assocv 'binfo pair-info)])
           `(pair ,(get-type-with-info at ainfo)
                  ,(get-type-with-info bt binfo)))
         `(pair ,at ,bt))]
    [`(array ,type)
     (define array-info (assocv 'array-info var-info))
     (if array-info
         (let ([size (assocv 'size array-info)]
               [typeinfo (assocv 'elem-info array-info)])
           `(array ,(get-type-with-info type typeinfo) (size . ,size)))
         `(array ,type))]))

(define (assocv sym lst (default #f))
  (if (and (list? lst) (andmap pair? lst))
      (let ([av (assoc sym lst)])
        (if av (cdr av) default))
      default))
(define (assocvr sym lst)
  (for/fold [(nlst '())]
            [(p (reverse lst))]
    (if (equal? (car p) sym)
        nlst
        (cons p nlst))))
