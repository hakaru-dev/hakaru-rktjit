#lang racket

(require sham/jit)
(require sham/private/ast)

(require "reduce-curry.rkt")
(require "parse-sexp.rkt")
(require "log-float.rkt")
(require "interpret.rkt")
(require "flatten.rkt")
(require "expand-lc.rkt")
(require "basic-fluff.rkt")
(require "ast.rkt")
(require "utils.rkt")

(provide (all-defined-out))

(define (read-file filename)
  (call-with-input-file filename
    (lambda (in)
      (read in))))


(define (passes interpret-args)
  (list (cons reduce-curry pretty-display)
        (cons parse-sexp (compose pretty-display print-expr))
        (cons flatten-anf (compose pretty-display print-expr))
        ;; (interpret interpret-args)
        (cons expand-to-lc (compose pretty-display print-ast))
        (cons add-fluff pretty-display)
        ))

(define (debug-program prg cmplrs)
  (define prog-ast
   (for/fold ([prg prg])
             ([c cmplrs])
     (define compiler (car c))
     (define printer (cdr c))
     (printf "\n\napplying ~a\n" (object-name compiler))
     (let ([p (compiler prg)])
       (unless (member (object-name compiler) '(reduce-curry; flatten-anf add-fluff
                                                ))
         (parameterize ([pretty-print-current-style-table
                         (pretty-print-extend-style-table
                          (pretty-print-current-style-table)
                          '(block define-variables define-function assign while)
                          '(begin let lambda set! do))]
                        [pretty-print-columns 100])
           (printer p)))
       p)))
  (define module-env (compile-module prog-ast))
  (jit-optimize-module module-env #:opt-level 3)
  (jit-dump-module module-env)
  (define mod-env (initialize-jit module-env #:opt-level 3))
  ;; (jit-dump-module mod-env)
  mod-env)

(define debug-pass (make-parameter #f))
(define (compile-src src)
  (define prog
    (for/fold ([prg src])
              ([c (list reduce-curry   parse-sexp expand-to-lc add-fluff)]
               [p (list pretty-display
                        (compose pretty-display print-expr)
                        (compose pretty-display print-ast)
                        pretty-display)])
      (define n (c prg))
      (printf "applying: ~a\n" (object-name c))
      (when (debug-pass)
        (printf "pretty-print:\n")
        (p n))
      n))
  (define module-env (compile-module prog))
  (jit-optimize-module module-env #:opt-level 3)
  (initialize-jit module-env #:opt-level 3))

(module+ test
  (require ffi/unsafe)
  (define topic-prior
    (vector-immutable
     0.014228 0.003821 0.030999  0.002363
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
     0.010813 0.029945 0.000568))
  (define word-prior (vector-immutable 0.023754 0.277952 0.698294))
  (define z (vector-immutable 1 0 2 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0))
  (define w
    (vector-immutable
     40 0 14 0 55 0 68 0 50 0 67 0 44 0 31 0
     89 0 66 0 29 0 43 0 57 0 69 0 75 0 77
     0 44 0 39 0 40 0 97 0 53 0 96 0 43 0
     26 0 36 0 8 0 78 0 94 0 82 0 8 0 77 0
     55 0 7 0 86 0 42 0 95 0 10 0 94 0 22
     0 73 0 30 0 88 0 14 0 90 0 18 0 47 0
     52 0 26 0 30 0 76 0 74 0 92 0 20 0 63
     0 50 0 74 0 51 0 81 0 76 0 60 0))
  (define doc
    (vector-immutable
     0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
     0 0 0 0 0 0 0 0 0 0 0 0 0 0
     0 0 0 0 0 0 0 0 0 0 0 1 0 1
     0 1 0 1 0 1 0 1 0 1 0 1 0 1
     0 1 0 1 0 1 0 1 0 1 0 1 0 1
     0 1 0 1 0 1 0 1 0 2 0 2 0 2
     0 2 0 2 0 2 0 2 0 2 0 2 0 2
     0 2 0 2 0 2 0 2 0 2 0 2 0 2
     0 2 0 2 0 2 0))
  (define doc-update 90)
  (define nbgo-src (read-file "../hkr/nb_simp.hkr"))
  (define nbgo-mod-jit
    (debug-program nbgo-src
                   (passes
                    (list topic-prior
                          word-prior
                          z
                          w
                          doc
                          doc-update))))
  ;; (jit-dump-module nbgo-mod-jit)

  (define nbgo-main (jit-get-function 'main nbgo-mod-jit))
  (define prob-type (jit-get-racket-type (env-lookup 'prob nbgo-mod-jit)))
  (define nat-type (jit-get-racket-type (env-lookup 'nat nbgo-mod-jit)))
  (define make-array-prob (jit-get-function 'make-array-prob nbgo-mod-jit))
  (define make-array-nat (jit-get-function 'make-array-nat nbgo-mod-jit))
  (define get-array-prob (jit-get-function 'get-array-prob nbgo-mod-jit))
  (define size-array-prob (jit-get-function 'size-array-prob-p nbgo-mod-jit))  

  (define (call-main a1 a2 a3 a4 a5 a6)
    (let ([ca1 (make-array-prob (vector-length a1) (vector->cblock a1 prob-type))]
          [ca2 (make-array-prob (vector-length a2) (vector->cblock a2 prob-type))]
          [ca3 (make-array-nat  (vector-length a3) (vector->cblock a3 nat-type))]
          [ca4 (make-array-nat  (vector-length a4) (vector->cblock a4 nat-type))]
          [ca5 (make-array-nat  (vector-length a5) (vector->cblock a5 nat-type))])
      (printf "making array done\n")
      (time
       (nbgo-main ca1 ca2 ca3 ca4 ca5 a6))))
  (define (get-racket-value v)
    (cblock->vector (get-array-prob v) prob-type (vector-length doc)))

  (begin
    (define result_raw
      (call-main topic-prior word-prior z w doc doc-update))

    (define result (get-racket-value result_raw))
    (pretty-display (vector-map exp result)))

  (define (read-from-csv fname)
    (call-with-input-file fname
      (lambda (in)
        (for/vector [(s (in-lines in))]
          (string->number s)))))

  (define arg1 (build-vector 20 (const 1.0)))
  (define arg2 (build-vector 7022 (const 1.0)))
  (define arg3 (read-from-csv "../arg3.csv")) ;;size 400   ;; values 0-20
  (define arg4 (read-from-csv "../arg4.csv")) ;;size 47049 ;; values 0-7022
  (define arg5 (read-from-csv "../arg5.csv")) ;;size 47049 ;; values 0-400
  (define arg6 0) ;; value 0-400
  (define big_res (call-main arg1 arg2 arg3 arg4 arg5 arg6))
  (pretty-print (vector-map exp (get-racket-value big_res))))

;; [1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0]
