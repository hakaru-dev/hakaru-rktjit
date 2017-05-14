#lang racket
(require "basic-defines.rkt")

(provide add-fluff)

(define (add-fluff fns)
  `(#%module
    (#:pass AlwaysInliner)
    ,@(basic-defines)
    ,@fns))
