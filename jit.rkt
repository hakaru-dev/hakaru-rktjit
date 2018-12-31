#lang racket

(require sham
         sham/jit-utils
         "ast.rkt"
         "pass.rkt"
         "pass/utils.rkt"
         "parse-hk.rkt"
         "utils.rkt")

(provide compile-file
         debug-file
         compile-function
         get-function)

(define basic-pass-list
  (list
   initial-simplifications    debug-print stop
   flatten-anf                ;; debug-print ;; stop
   later-simplifications      ;; debug-print ;; stop
   middle-simplifications     ;; debug-print ;; stop
   later-simplifications      ;; debug-print ;; stop
   fix-loop-lets              ;; debug-print ;; stop
   combine-loops              ;; debug-print ;; stop
   later-simplifications      ;; debug-print ;; stop
   remove-pairs               ;; debug-print ;; stop

   pull-indexes               ;; debug-print ;; stop

   later-simplifications      ;; debug-print ;; stop

   to-stmt                    ;; debug-print ;; stop
   compile-opts               debug-print ;; stop
   expand-to-sham             debug-print ;;stop
   compile-with-sham          ;; debug-print ;; stop
   ))
(define passes
  `(,clean-curry
    ,parse-sexp ;; ,debug-print ;; stop
    ,@basic-pass-list))

(define (run-pipeline src arg-info)
  (define init-state
    (state src
           (make-immutable-hash (list (cons prog-arg-info arg-info)))
           passes))
  (define-values (env info) (run-state init-state))
  env)

;; this arginfo for now only talks about the prog function,
;; as for hakaru we only take one function
;; arginfo is a list of same size as args
;; the position in list maps to the info about
;; the argument of the function

;; each info is again list
;; which can have following:

;; (constant <value>)
;; (pair-info <ainfo> <binfo>)
;; (array-info (size <value>) (valuerange (from . to))
;; (curryhere)

(define (compile-src src arg-info)
  (run-pipeline src arg-info))

(define (compile-file fname arg-info)
  (compile-src (file->value fname) arg-info))

(define (get-function sham-module fid)
  (sham-module-lookup-function sham-module fid))

(define (compile-function prog-expr prog-info)
  (define-values (env info)
    (run-state
     (state
      (list prog-expr)
      (make-immutable-hash (list (cons prog-arg-info prog-info)))
      basic-pass-list)))
  env)

(define (debug-file fname arg-info)
  (parameterize ([hakrit-print-debug #t]
                 [debug-curry #t]
                 [debug-flatten-anf #t]
                 [debug-combine-loops #t]
                 [debug-later-simplifications #t]
                 [debug-to-sham #t]
                 [debug-print-stop #t])
    (compile-src (file->value fname) arg-info)))


(define (debug-store-file src-fname out-fname)
  (call-with-output-file out-fname
    (λ (out-port)
      (parameterize ([current-output-port out-port])
        (compile-file src-fname)))
    #:exists 'truncate/replace))

(module+ test
  (require ffi/unsafe)
  ;; (require disassemble)
  (define test-val (interpret-file "../test/simple/array-product.hkr" (list (build-vector 10 (const 1.0)))))

  ;; (define sham-module (debug-file "../test/simple/array-product.hkr" '()))
  ;; (define prog (get-function sham-module 'prog))
  )
