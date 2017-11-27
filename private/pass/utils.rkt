#lang racket

(require sham
         racket/splicing
         "../utils.rkt"
         "ast.rkt")

(provide (all-defined-out)
         (all-from-out "../utils.rkt"))

;; converts an expression to a simple statment with the value given to
;; assign-to function at the end
(define (expr->stmt e assign-to)
  (define (ers e)
    (match e
      [(expr-if typ tst thn els)
       (stmt-if tst (ers thn) (ers els))]
      [(expr-lets types vars vals s body)
       (stmt-expr (stmt-void)
                  (expr-lets types vars vals (stmt-block (list s (ers body))) (expr-val 'nat 0)))]
      [else (assign-to e)]))
  (ers e))

(splicing-let ([gensym-hash (make-hash)])
  (define (gensym^ sym [sep ""])
    (define n (add1 (hash-ref gensym-hash sym 0)))
    (hash-set! gensym-hash sym n)
    (string->symbol (string-append (symbol->string sym) sep (number->string n)))))

(define (assocv sym lst (default #f))
  (if lst
      (let ([av (assoc sym lst)])
        (if av (cdr av) default))
      default))
(define (assocvr sym lst)
  (for/fold [(nlst '())]
            [(p (reverse lst))]
    (if (equal? (car p) sym)
        nlst
        (cons p nlst))))

(define (add-array-size-info t s)
  (match t
    [`(array ,tar) `(array ,tar (size . ,s))]
    [`(array ,tar (size . ,sd))
     #:when (not (equal? s sd))
     (error "changing array size of a type")]))
(define ((debug-printf param) . args)
  (when (param)
    (apply printf args)))


(struct state (prg info rest-pass))
(define (run-next new-prg new-info old-state)
  (match old-state
    [(state _ _ '())
     (values new-prg new-info)]
    [(state _ _ rest-pass)
     ((car rest-pass) (state new-prg new-info (cdr rest-pass)))]))

(define (run-state st)
  (match st
    [(state prg info passes)
     ((car passes) (state prg info (cdr passes)))]))

(define (run-next-state st)
  (match st
    [(state prg info passes)
     ((second passes) (state prg info (cddr passes)))]))

(define (vector-getpos vec f?)
  (map cadr (filter (λ (vi) (f? (car vi)))  (map list (vector->list vec) (build-list (vector-length vec) identity)))))

;;info syms
(define prog-arg-info 'arg-info)

(define (build-var-info arg-info)
  `((arg-info . ,arg-info)))
(define (constant-size-array? t)
  (match t
    [`(array ,_ ... (size . ,size)) #t]
    [else #f]))

(define (get-size-of-array t)
  (match t
    [`(array ,_ ... (size . ,size)) size]
    [else (error "getting size of an array type whose size we don't know")]))


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
    ['bool (number-type)]
    [`(pair ,at ,bt)
     (define pair-info (assocv 'pairinfo var-info))
     (if pair-info
         (let ([ainfo (assocv 'ainfo pair-info)]
               [binfo (assocv 'binfo pair-info)])
           `(pair ,(get-type-with-info at ainfo)
                  ,(get-type-with-info bt binfo)))
         `(pair ,at ,bt))]
    [`(array ,type)
     (define array-info (assocv 'arrayinfo var-info))
     (if array-info
         (let ([size (assocv 'size array-info)]
               [typeinfo (assocv 'typeinfo array-info)])
           `(array ,(get-type-with-info type typeinfo) (size . ,size)))
         `(array ,type))]))

(define (debug-print st)
  (match st
    [(state prg info os)
     (if (list? prg)
         (if (expr? (car prg))
             (printf "debug-printing multiple: \n~a\n" (map (compose pretty-format pe) prg))
             (printf "debug-printing multiple: \n~a\n" (map (compose pretty-format print-sham-def) prg)))
         (printf "debug-printing: \n~a\n" (pretty-format (pe prg))))
     (run-next prg info st)]))
