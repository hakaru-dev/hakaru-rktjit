#lang racket

(require sham/jit)
(require "basic-defines.rkt")

(provide (all-defined-out))

(define (read-file filename)
  (call-with-input-file filename
    (lambda (in)
      (read in))))






(define compilers (list reduce-function
                        simplify-expr
                        flatten-expr
                        expand-spa
                        add-fluff))

(define (debug-program prg cmplrs)
  (define prog-ast
   (for/fold ([prg prg])
             ([c cmplrs])
     (parameterize ([pretty-print-current-style-table
                     (pretty-print-extend-style-table
                      (pretty-print-current-style-table)
                      '(block define-variables define-function assign while)
                      '(begin let lambda set! do))]
                    [pretty-print-columns 100])
       (pretty-display prg))
     (printf "\n\napplying ~a\n" (object-name c))
     (c prg)))
  (pretty-display prog-ast)
  (define mod-env (initialize-jit (compile-module prog-ast)))
  (jit-dump-module mod-env)
  mod-env)

(define (compile-src src)
  (debug-program src compilers))
(module+ test
  (require ffi/unsafe)

  (require "../disassemble/disassemble/main.rkt")
  ;; (disassemble-ffi-function fun #:size 100)

  (define hello-src (read-file "examples/hello-full.hkr"))
  (define nbg-src (read-file "examples/naive-bayes-gibbs-full.hkr"))
  (define nbgo-src (read-file "examples/naive-bayes-gibbs-opt.hkr"))
  ;; (debug-program hello-src compilers)
  ;; (define hello-env (debug-program hello-src compilers))
  ;; (define (get-hello-f f)
  ;;   (jit-get-function (env-lookup f hello-env)))
  ;; (define test-array ((get-hello-f 'make-array-real)  4
  ;;                     (list->cblock `(80.0 20.1 30.2 40.4 ) real-type)))


  ;; (printf "test value: ~a\n"((get-hello-f 'f) test-array))

  (define nbgo-mod-jit (debug-program nbgo-src compilers))
  ;; (jit-dump-module nbgo-mod-jit)
  (define nbgo-main (jit-get-function 'main nbgo-mod-jit))

  (define prob-type (jit-get-racket-type (env-lookup 'prob nbgo-mod-jit)))
  (define nat-type (jit-get-racket-type (env-lookup 'nat nbgo-mod-jit)))

  (define make-array-prob (jit-get-function 'make-array-prob nbgo-mod-jit))
  (define make-array-nat (jit-get-function 'make-array-nat nbgo-mod-jit))

  (define topic-prior
    (make-array-prob 100
                     (list->cblock '(0.014228 0.003821 0.030999 0.002363
                                              0.024379 0.001317 0.002707
                                              0.013426 0.000219 0.008970
                                              0.012040 0.003862 0.006986
                                              0.012392 0.004347 0.003219
                                              0.011661 0.006158 0.011185
                                              0.060091 0.008681 0.000573
                                              0.019064 0.010391 0.005424
                                              0.022017 0.015388 0.001420
                                              0.023657 0.007121 0.003429
                                              0.004026 0.005844 0.003225
                                              0.045181 0.006403 0.018045
                                              0.006239 0.008657 0.000347
                                              0.002385 0.000023 0.005600
                                              0.032088 0.005185 0.014991
                                              0.003511 0.001131 0.004627
                                              0.002341 0.013252 0.016136
                                              0.017101 0.003137 0.003582
                                              0.000282 0.000733 0.018463
                                              0.004997 0.000185 0.029280
                                              0.005629 0.018106 0.016644
                                              0.013107 0.002684 0.019586
                                              0.004801 0.020799 0.008960
                                              0.013165 0.000553 0.000053
                                              0.001443 0.000540 0.008137
                                              0.005964 0.004829 0.005559
                                              0.004122 0.000518 0.018653
                                              0.022602 0.007208 0.007697
                                              0.037427 0.002005 0.002432
                                              0.012954 0.000968 0.001409
                                              0.005069 0.019474 0.015708
                                              0.001842 0.009659 0.009836
                                              0.010813 0.029945 0.000568)
                                   prob-type)))
  (define word-prior
    (make-array-prob 3 (list->cblock '(0.023754 0.277952 0.698294) prob-type)))
  (define z
    (make-array-nat 20 (list->cblock '(1 0 2 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 )
                                     nat-type)))
  (define w
    (make-array-nat 120 (list->cblock '(40 0 14 0 55 0 68 0 50 0 67 0 44 0 31 0
                                           89 0 66 0 29 0 43 0 57 0 69 0 75 0 77
                                           0 44 0 39 0 40 0 97 0 53 0 96 0 43 0
                                           26 0 36 0 8 0 78 0 94 0 82 0 8 0 77 0
                                           55 0 7 0 86 0 42 0 95 0 10 0 94 0 22
                                           0 73 0 30 0 88 0 14 0 90 0 18 0 47 0
                                           52 0 26 0 30 0 76 0 74 0 92 0 20 0 63
                                           0 50 0 74 0 51 0 81 0 76 0 60 0 )
                                      nat-type)))
  (define doc (make-array-nat 120 (list->cblock '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                                                    0 0 0 0 0 0 0 0 0 0 0 0 0 0
                                                    0 0 0 0 0 0 0 0 0 0 0 1 0 1
                                                    0 1 0 1 0 1 0 1 0 1 0 1 0 1
                                                    0 1 0 1 0 1 0 1 0 1 0 1 0 1
                                                    0 1 0 1 0 1 0 1 0 2 0 2 0 2
                                                    0 2 0 2 0 2 0 2 0 2 0 2 0 2
                                                    0 2 0 2 0 2 0 2 0 2 0 2 0 2
                                                    0 2 0 2 0 2 0 )
                                                nat-type)))
  ;; (define f (get-nbg-f 'f))
  (define result_raw (time (nbgo-main topic-prior word-prior z w doc 1)))
  (define get-array-prob (jit-get-function 'get-array-prob nbgo-mod-jit))
  (define result (cblock->list (get-array-prob result_raw) prob-type 120))
  (pretty-display result)
  )
