#lang racket

(provide (all-defined-out))
;;TODO at some point we want to move all this to a
;; language one level above sham lc2 or lc1  and make the basic sham lang lc1 or lc0

(define pointer-format "~a*")
(define array-format "array<~a>")
(define sized-array-format "array<~a.~a>")
(define array-args '(size data))

(define pair-format "pair<~a.~a>")
(define pair-car-sym 'a)
(define pair-cdr-sym 'b)

(define function-init-char "$")
(define function-sep-char "&")
(define function-end-char "")
(define (create-function-template name num-args)
  (string-append name
                 function-init-char
                 (apply ~a (build-list num-args (λ (_) "~a")) #:separator function-sep-char)
                 function-end-char))
(define cft create-function-template)

(define reject-fun-format (cft "reject" 1))
(define eq-dif-type (cft "eq" 2))
;;pair-funs
(define make-pair-fun-format (cft "make" 1))
(define pair-car-fun-format (cft "car" 1))
(define pair-cdr-fun-format (cft "cdr" 1))
(define pair-set-car-fun-format (cft "set-car!" 1))
(define pair-set-cdr-fun-format (cft "set-cdr!" 1))

;struct
(define struct-literal-fun-format (cft "literal" 1))
(define struct-make-fun-format (cft "make" 1))
(define struct-get-index-fun-format (cft "index" 1))

;;array-funs
(define make-array-fun-format (cft "make" 1))
(define new-size-array-fun-format (cft "new-sized" 1))
(define free-size-array-fun-format (cft "free-sized" 1))
(define empty-array-fun-format (cft "empty" 1))
(define clear-size-array-fun-format (cft "clear" 1))
(define get-array-size-fun-format (cft "get-size" 1))
(define get-array-data-fun-format (cft "get-data" 1))

(define get-index-fun-format (cft "get-index" 1))
(define get-index-error-fun-format (cft "get-index-error" 1))
(define rkt-get-index-error-fun-format (cft "rkt-get-index-error" 1))

(define set-index-fun-format (cft "set-index!" 1))
(define set-index-error-fun-format (cft "set-index-error!" 1))
(define rkt-set-index-error-fun-format (cft "rkt-set-index-error!" 1))

(define array-literal-fun-format (cft "array-literal" 2))
(define size-array-literal-fun-format (cft "literal" 1))

(define add-fun-format (cft "add" 2));;add <num-args> <type>
(define mul-fun-format (cft "mul" 2));;mul <num-args> <type>
(define recip-fun-format (cft "recip" 1))

(define (get-fun-symbol frmt . args)
  (string->symbol (apply format (cons frmt args))))
