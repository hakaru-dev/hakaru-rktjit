#lang racket

(define (add-fluff fns)
  `(#%module
    (#:pass AlwaysInliner)
    ,@(basic-defines)
    ,@fns))
