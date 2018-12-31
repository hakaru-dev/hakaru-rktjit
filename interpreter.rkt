#lang racket

(require "pass/utils.rkt"
         "pass/all.rkt")

(provide interpret-file)
(define (interpret-file fname arg-vals)
  (run-state (state (file->value fname)
                    (make-immutable-hash (list (cons prog-arg-info '())))
                    (list clean-curry parse-sexp (interpret arg-vals)))))
(module+ test
  (require ffi/unsafe)
  ;; (require disassemble)
  (define test-val (interpret-file "../test/simple/array-product.hkr" (list (build-vector 10 (const 1.0)))))

  ;; (define sham-module (debug-file "../test/simple/array-product.hkr" '()))
  ;; (define prog (get-function sham-module 'prog))
  )
