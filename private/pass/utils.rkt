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

(define (array-type? t)
  (and (pair? t) (equal? (car t) 'array)))
(define (array-element-type t) (second t))

(define (constant-size-array? t)
  (match t
    [`(array ,_ ... (size . ,size)) #t]
    [else #f]))
(define (get-size-of-array t)
  (match t
    [`(array ,_ ... (size . ,size) ,i ...) size]
    [else (error "getting size of an array type whose size we don't know")]))



(define (is-constant-type? t)
  (if (list? t)
      (assocv 'constant (cdr t))
      #f))
(define (get-constant-value t)
   (assocv 'constant (cdr t)))


(define (get-arg-info i) (assocv 'arg-info i))
(define (get-info-attrs i)
  (assocv 'attrs i '()))
(define (get-array-info i)
  (assocv  'array-info (get-arg-info i)))
(define (get-array-elem-info i)
  (assocv 'elem-info (get-array-info i)))
(define (get-array-elem-type-info i t)
  (assocv t (get-array-elem-info i)))
(define (get-array-elem-constant-value i t)
  (assocv 'constant (get-array-elem-type-info i t)))


(define (list-box [init-list '()])
  (box init-list))
(define (add-to-box! list-box elem)
  (set-box! list-box (cons elem (unbox list-box))))
