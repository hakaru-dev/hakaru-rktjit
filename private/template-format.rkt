#lang racket

(provide (all-defined-out))

(define pointer-format "~a*")
(define array-format "array<~a>")
(define array-args '(size data))

(define pair-format "pair<~a,~a>")
(define pair-car-sym 'a)
(define pair-cdr-sym 'b)

(define function-init-char ":")
(define function-sep-char ",")
(define function-end-char ".")
(define (create-function-template name num-args)
  (string-append name
                 function-init-char
                 (apply ~a (build-list num-args (λ (_) "~a")) #:separator function-sep-char)
                 function-end-char))
(define cft create-function-template)

;;pair-funs
(define make-pair-fun-format (cft "make" 1))
(define pair-car-fun-format (cft "car" 1))
(define pair-cdr-fun-format (cft "cdr" 1))

;;array-funs
(define make-array-fun-format (cft "make" 1))
(define new-size-array-fun-format (cft "new-sized" 1))
(define empty-array-fun-format (cft "empty" 1))
(define get-array-size-fun-format (cft "get-size" 1))
(define get-array-data-fun-format (cft "get-data" 1))
(define get-index-fun-format (cft "get-index" 1))
(define set-index-fun-format (cft "set-index" 1))
(define array-literal-fun-format (cft "array-literal" 2))

(define add-fun-format "add-~a-~a");;add-<num-args>-<type>
(define mul-fun-format "mul-~a-~a");;mul-<num-args>-<type>
(define recip-fun-format "recip-~a")

(define (get-fun-symbol frmt . args)
  (string->symbol (apply format (cons frmt args))))
