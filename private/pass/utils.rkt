#lang racket

(require racket/splicing
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

(define (assocv sym lst)
  (if lst
      (let ([av (assoc sym lst)])
        (if av (cdr av) #f))
      #f))

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
