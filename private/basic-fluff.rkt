#lang racket

(require sham/private/ast)
(require "basic-defines.rkt")

(provide add-fluff)

(define (add-fluff fns)
  `(#%module
    (#:pass AlwaysInliner)
    ,@(basic-defines)
    ,@(map ast->sexp (ast-module-defs fns))))
