#lang racket

(require sham
         "ast.rkt"
         "pass.rkt"
         "utils.rkt")

(provide compile-file
         debug-file)

(define pp-expr (compose pretty-display print-expr))
(define pp-sham (compose pretty-display print-sham-module))
(define (pp-prog mod)
  (for ([def (sham:module-defs mod)])
    (when (equal? (sham:def-id def) 'prog)
      (pretty-display (print-sham-def def)))))

;(define pp-expr (const #f))
;(define pp-sham (const #f))

(define stop (cons (λ (e) (error 'stop))
                   pp-expr))
(define passes
  (list (cons reduce-curry            pretty-display)
        (cons parse-sexp              pp-expr)

        (cons initial-simplifications pp-expr)

        (cons flatten-anf             pp-expr)
        (cons combine-loops           pp-expr)

        (cons later-simplifications   pp-expr)

        (cons pull-indexes            pp-expr);;better loop fusion
        (cons later-simplifications   pp-expr)

        (cons to-stmt                 pp-expr)

        (cons expand-to-lc            pp-prog)))


(define to-print? (make-parameter (const #t)))
(define to-not-print? (make-parameter (const #f)))

(define (debug-print compiler printer new-p)
  (when (debug-pass)
    (define pass-name (object-name compiler))
    (printf "applying-pass: ~a\n" pass-name)
    (cond
      [((to-print?) pass-name) (printer new-p)]
      [((to-not-print?) pass-name) (void)]
      [else (printer new-p)])))


(define (compile-src src)
  (define prog-module
   (for/fold ([prg src])
             ([p passes])
     (define compiler (car p))
     (define new-p (compiler prg))
     (debug-print compiler (cdr p) new-p)
     new-p))

  (define module-env (compile-module prog-module))
  (jit-optimize-module module-env #:opt-level 3)
  (jit-optimize-function module-env #:opt-level 3)
  ;(jit-dump-module module-env)
  ;(jit-dump-function module-env 'prog)

  ;(jit-verify-module module-env)
  (initialize-jit module-env #:opt-level 3))

(define compile-file (compose compile-src file->value))

(define (debug-src src)
  (parameterize
      ([debug-pass #t]
       [to-print?
        (curryr
         member
         '(;reduce-curry
           ;parse-sexp
           ;; initial-simplifications
           ;; flatten-anf
           ;; combine-loops
           ;; later-simplifications
            to-stmt
            expand-to-lc))]
       [to-not-print? (const #f)])
    (compile-src src)))

(define debug-file (compose debug-src file->value))

(define (debug-store-file src-fname out-fname)
  (call-with-output-file out-fname
    (λ (out-port)
      (parameterize ([current-output-port out-port])
        (debug-file src-fname)))
    #:exists 'truncate/replace))

(module+ test
  (require ffi/unsafe)
  (define module-env (debug-file "../../testcode/hkrkt/ClinicalTrial.hkr"))
  (require disassemble)
  (jit-dump-module module-env)
  (jit-verify-module module-env)
  (define (get-f sym)
    (jit-get-function sym module-env))
  (define prog (jit-get-function 'prog module-env))
  (define init-rng (jit-get-function 'init-rng module-env))
  (init-rng)
  (define tbool (jit-get-racket-type 'bool module-env))
  (define make-boolarr-pair (jit-get-function 'make$pair<array<bool>*.array<bool>*> module-env))
  (define make-bool-array (jit-get-function 'make$array<bool> module-env))
  (define carb (get-f 'car$pair<array<bool>*.array<bool>*>))
  (define cdrb (get-f 'cdr$pair<array<bool>*.array<bool>*>))
  (define gib  (get-f 'get-index$array<bool>))
  (define sib (get-f 'set-index!$array<bool>))
  (define gsb (get-f 'get-size$array<bool>))
  (define (make-carray f t l)
    (f (length l) (list->cblock l t)))
  (define n 1000)
  (define a
   (make-carray
    make-bool-array tbool
    '(0 1 1 0 1 0 0 1 1 1 1 1 1 1 1 1 0 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 0 1 1 0 1 0 1 1 1 1 1 0 1 1 1
       1 1 1 1 1 1 1 1 0 0 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 0 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 0 0
       1 1 1 1 0 1 1 1 1 0 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 1 1 1 1 1 1 0 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 0 0 1 1 0 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 0 0 1 0 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 0 1 1 1 1 1
       1 1 1 1 1 1 0 1 1 1 1 0 1 1 1 0 1 1 0 1 1 0 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 0 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 0 0 1 1 0 1 1 1 0 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 0 1 1 1 1 1 1 1 1 0 1 1 1 0 0 0
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 1 0 0 0 1 1 1 1 1 1 1 1 1 0 1 0 1 1 0 1 0 1 1 1 0 1 1 1 1 1 0 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0
       1 0 1 1 1 1 1 1 0 1 1 1 1 1 1 0 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 0 1 1 0 1 1 1 1 1 1 1 1
       0 0 0 1 1 0 1 1 1 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 1 1 1
       1 1 1 1 0 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 0 1 0 1 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 0 1 1 1 1
       1 0 1 1 1 1 1 1 1 0 1 1 1 1 1 0 0 1 1 1 0 1 1 1 0 0 1 1 1 0 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 0 1 1 1 1 1 0 1 1 1 0 0 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 0 0 1 1 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1
       1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 0 1 0
       1 1 1 1 1 0 0 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 0 1 1 0 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 0 0 1 1 1 1 1 1 1 1 1 1 1 1 0 1 0 0 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 0 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 0 1 1 1 1 1 0 1 1 0 1 1 1 1 1 0 1
       0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 0 1 0 1 1 1 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 0 1 1 0 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 0 1 0 1 1 1 1)))
  (define b
    (make-carray
     make-bool-array tbool
     '(0 0 1 0 1 1 1 0 1 1 0 1 1 1 1 1 0 1 1 0 0 1 1 0 1 1 1 1 0 1 0 1 1 0 0 0 1 1 1 1 1 1 1 1 1 1 0 0 0 1 0 1 0 0
         0 1 1 1 0 1 1 0 1 0 1 0 1 1 0 0 1 0 1 1 1 1 1 0 1 1 1 1 1 1 0 1 0 0 1 1 0 0 1 1 1 1 0 0 1 1 1 1 1 0 0 0 0
         1 1 0 0 0 1 1 0 1 0 1 0 0 1 0 1 0 1 1 1 1 0 1 0 1 0 1 1 0 1 1 1 0 1 0 0 1 0 0 1 1 0 0 1 0 1 0 1 1 1 0 0 0
         1 1 0 1 1 1 1 1 0 0 1 1 0 0 0 1 0 1 0 0 1 1 0 1 1 1 0 1 1 1 0 1 1 0 0 0 1 1 0 1 1 1 0 1 0 1 1 1 1 1 1 0 1
         0 1 1 0 0 1 0 0 1 1 1 0 1 1 1 0 0 0 0 1 0 0 1 0 1 0 0 1 1 1 0 1 1 1 0 0 1 0 1 0 1 1 1 1 1 1 1 0 1 0 1 0 1
         0 0 1 0 0 0 0 1 1 1 0 1 0 1 1 0 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 0 1 0 0 1 1 0 0 1 0 0 0 1 0 0 1
         0 1 0 0 0 1 1 0 1 1 1 0 1 1 0 0 1 0 0 0 1 0 0 1 0 1 1 1 0 1 1 0 1 1 1 1 1 1 1 1 0 1 0 1 0 1 1 0 1 0 1 1 0
         1 1 0 0 1 1 0 1 1 1 0 0 0 1 0 0 0 1 1 1 1 0 0 1 1 0 0 1 0 0 1 1 1 1 1 1 0 0 1 1 1 0 0 1 1 0 0 1 0 0 1 0 0
         1 0 0 1 0 1 0 0 1 1 0 1 1 0 0 1 0 1 1 0 0 1 0 0 1 0 0 1 0 1 0 1 1 0 0 1 1 1 1 1 0 0 0 1 1 0 1 1 1 1 1 1 1
         1 0 1 1 0 0 1 1 0 0 1 1 0 1 0 1 0 1 0 1 1 0 0 0 1 1 1 0 1 0 0 1 0 1 0 0 1 1 1 0 0 0 1 1 1 0 0 0 0 1 1 0 1
         1 1 0 0 0 1 0 1 1 1 1 1 1 1 1 0 0 1 0 1 1 0 1 0 1 1 0 0 1 1 1 1 1 1 0 0 1 1 1 0 0 0 0 1 0 1 1 0 1 1 0 0 1
         1 1 1 1 1 0 0 0 1 0 1 1 1 1 1 1 0 1 1 0 0 1 1 1 0 1 0 1 0 1 1 0 0 1 1 0 1 0 0 1 0 1 1 1 1 0 1 1 1 0 0 1 0
         1 1 1 1 1 1 0 0 1 1 1 1 1 1 1 0 0 1 0 0 1 1 0 0 1 0 0 1 1 1 1 1 0 0 1 0 0 1 0 1 0 0 0 1 1 0 0 0 0 0 0 0 0
         1 1 1 1 1 0 0 1 0 1 1 1 0 1 0 1 0 1 1 0 1 1 0 1 0 0 1 0 0 0 0 1 0 1 1 1 1 1 0 1 1 1 1 0 1 1 1 1 0 1 0 1 1
         0 0 0 0 0 0 1 1 1 0 1 1 0 1 1 0 0 0 0 1 0 0 1 1 1 0 0 1 1 0 1 0 0 1 1 1 1 1 1 0 1 1 0 1 1 1 1 1 0 1 1 1 1
         0 0 0 1 1 1 1 0 1 0 1 1 0 1 1 1 1 0 0 1 0 0 0 0 1 1 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0 1 1 1 1 0 1 1 0 0 0 1 1
         1 1 0 1 1 0 1 0 1 0 1 0 0 1 1 1 1 1 1 0 1 0 0 0 1 1 0 1 1 0 1 1 0 1 0 1 1 0 0 0 1 0 1 1 0 1 1 1 1 0 1 1 1
         0 1 0 1 1 1 1 1 1 0 0 0 1 0 1 0 1 1 0 1 1 1 0 0 0 0 1 0 1 1 1 1 1 0 1 0 0 1 1 1 1 1 1 0 1 0 0 1 1 1 1 0 1
         1 1 0 1 0 1 1 0 0 1 1 0 1 1 0 0 0 1 0 1 0 1 0 1 0 1 1 0 0 0 1 1 1 1 1 0 1 0 0 1 1 0 0 0 0)))
  (define p (make-boolarr-pair a b))

  ;(disassemble-ffi-function (jit-get-function-ptr 'prog module-env) #:size 600)
  (prog n p))
  ;; (debug-file "../../testcode/hkrkt/linearRegression_simp.hkr")
  ;; (debug-file "../../testcode/hkrkt/gmm_gibbs_simp.hkr")
  ;; (debug-file "../../testcode/hkrkt/naive_bayes_gibbs_simp.hkr")
  ;; (debug-file "../../testcode/hkrkt/lda_gibbs_simp.hkr"))

  ;; (debug-store-file "../../testcode/hkrkt/clinicalTrial_simp.hkr"
  ;;                   ".testout/clinicalTrial.txt")

  ;; (debug-store-file "../../testcode/hkrkt/linearRegression_simp.hkr"
  ;;                   ".testout/linearregression.txt")
  ;; (debug-store-file "../../testcode/hkrkt/gmm_gibbs_simp.hkr"
  ;;                   ".testout/gmm_gibbs.txt")
  ;; (debug-store-file "../../testcode/hkrkt/naive_bayes_gibbs_simp.hkr"
  ;;                   ".testout/naive_bayes_gibbs.txt"))
  ;; (debug-store-file "../../testcode/hkrkt/lda_gibbs_simp.hkr"
  ;;                   ".testcode/hkrkt/lda_gibbs_simp.hkr")
#|
TODO, gsl_sf_beta is causing underflow, don't know why check with haskell tomorrow
probability-defines.rkt:test﻿> (realbetaFunc 1645.0 357.0)
printing: 1645.0
printing: 357.0
gsl: exp.c:545: ERROR: underflow
Default GSL error handler invoked.

Process Racket REPL aborted (core dumped)
|#
