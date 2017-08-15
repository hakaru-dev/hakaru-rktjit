#lang racket

(require sham/private/ast)
(require "basic-defines.rkt")

(provide add-fluff)

(define (add-fluff m)
  (match m
    [(sham:module p fns)
     (sham:module '(AlwaysInliner)
                  (append (basic-defines) fns))]))
