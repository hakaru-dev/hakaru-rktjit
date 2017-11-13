#lang racket
(require "../utils.rkt")

(provide (all-defined-out)
         (all-from-out "../utils.rkt"))

(define (get-vi i)
  (string->symbol (format "v~a" i)))
