#lang racket

(require sham
         sham/private/jit-utils
         sham/private/ast-utils)
(require "prelude.rkt"
         "utils.rkt")

(provide compile-with-sham debug-compile)

(define debug-compile (make-parameter #f))

(define (debug-compile-options)
  (append '(mc-jit)
          (if (debug-compile)
              '(pretty dump mc-jit verify)
              '())))
(define (create-module defs)
  (define defs-module (create-sham-module defs "hakrit" (basic-mod-info)))
  (define full-module (merge-sham-module defs-module prelude-module))
  (parameterize ([compile-options (debug-compile-options)])
    (compile-sham-module! full-module #:opt-level 1 #:size-level 1 #:loop-vec #t))
  full-module)

(define (compile-with-sham st)
  (match st
    [(state defs info os)
     (define mod-env (create-module defs))
     (run-next mod-env info st)]))
