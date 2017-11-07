#lang racket

(require racket/splicing
         "../utils.rkt"
         "ast.rkt")

(provide expr->stmt
         gensym^
         (all-from-out "../utils.rkt"))

;; converts an expression to statment with the value given to
;; assign-to function at the end
(define (expr->stmt e assign-to)
  (define (ers e)
    (match e
      [(expr-if typ tst thn els)
       (stmt-if tst (ers thn) (ers els))]
      ;; [(expr-let t var val body)
      ;;  (stmt-elets (list var) (list val) (ers body))]
      [(expr-lets types vars vals s body)
       (stmt-expr (stmt-void) (expr-lets types vars vals s (ers body)))]
      [else (assign-to e)]))
  (ers e))

(splicing-let ([gensym-hash (make-hash)])
  (define (gensym^ sym [sep ""])
    (define n (add1 (hash-ref gensym-hash sym 0)))
    (hash-set! gensym-hash sym n)
    (string->symbol (string-append (symbol->string sym)
                                   sep
                                   (number->string n)))))
