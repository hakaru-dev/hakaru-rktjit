#lang racket

(require sham)
(require "prelude.rkt"
         "utils.rkt")

(provide compile-with-sham
         optimize&init-jit)

(define (create-module defs)
  (define sham-module
    (sham:module
     (basic-mod-info)
     defs))
  (define mod-env (compile-module sham-module))
  ;; (jit-dump-module mod-env)
  ;; (jit-verify-module mod-env)
  mod-env)

(define (compile-with-sham st)
  (match st
    [(state defs info os)
     (run-next (create-module defs) info st)]))


(define (optimize&init-jit st)
  (match st
    [(state mod-env info os)
     (optimize-module mod-env)
     (initialize-jit! mod-env #:opt-level 3)
     mod-env]))
