#lang racket
(require "utils.rkt")

(provide clean-curry)

;; (define (src stop-at)
;;   (define (get-args-body prg [var-type-assoc null])
;;     (match prg
;;       [`((fn ,var ,type ,body) : ,t)
;;        (get-args-body body (cons (list var (car t)) var-type-assoc))]
;;       [else (values (reverse var-type-assoc) prg)]))
;;   (define-values (args fbody) (get-args-body expr))
;;   (match fbody
;;     [`(,body : ,type)
;;      `(fn ,args ,type ,fbody)]))

(define (clean-curry st)
  (void))
  ;; (match st
  ;;   [(state src info passes)
  ;;    (define pai (hash-ref info prog-arg-info))
  ;;    (define stops (vector-getpos (λ (arg-info) (if (set-member? arg-info 'curryhere) 42 0)) pai))
  ;;    (define new-src (curry-at-stops src stops))
  ;;    (run-next new-src info st)])
