#lang racket

(require sham/jit)

(require "reduce-curry.rkt")
(require "parse-sexp.rkt")
(require "log-float.rkt")
(require "interpret.rkt")
(require "flatten.rkt")
(require "expand-lc.rkt")
(require "basic-fluff.rkt")
(require "ast.rkt")

(provide (all-defined-out))

(define (read-file filename)
  (call-with-input-file filename
    (lambda (in)
      (read in))))


(define (passes interpret-args)
  (list reduce-curry
        parse-sexp
        flatten-anf        
        ;; (interpret interpret-args)

        expand-to-lc
        ;; add-fluff
        ))

(define (debug-program prg cmplrs)
  (define prog-ast
   (for/fold ([prg prg])
             ([c cmplrs])
     (unless (member (object-name c) '(reduce-curry parse-sexp))
       (parameterize ([pretty-print-current-style-table
                       (pretty-print-extend-style-table
                        (pretty-print-current-style-table)
                        '(block define-variables define-function assign while)
                        '(begin let lambda set! do))]
                      [pretty-print-columns 100])
         (pretty-display (print-expr prg))
         ))
     (printf "\n\napplying ~a\n" (object-name c))
     (c prg)))
  (pretty-display (print-expr prog-ast))
  (error 'stop)
  (define mod-env (initialize-jit (compile-module prog-ast)))
  (jit-dump-module mod-env)
  mod-env)

(module+ test
  (require ffi/unsafe)
  (define topic-prior-vector
    (vector-immutable
     0.14228 0.03821 0.30999 0.02363 0.24379
     0.01317 0.02707 0.13426 0.00219 0.08970
     0.12040 0.03862 0.06986 0.12392 0.04347
     0.03219 0.11661 0.06158 0.11185 0.60091
     0.08681 0.00573 0.19064 0.10391 0.05424
     0.22017 0.15388 0.01420 0.23657 0.07121
     0.03429 0.04026 0.05844 0.03225 0.45181
     0.06403 0.18045 0.06239 0.08657 0.00347
     0.02385 0.00023 0.05600 0.32088 0.05185
     0.14991 0.03511 0.01131 0.04627 0.02341
     0.13252 0.16136 0.17101 0.03137 0.03582
     0.00282 0.00733 0.18463 0.04997 0.00185
     0.29280 0.05629 0.18106 0.16644 0.13107
     0.02684 0.19586 0.04801 0.20799 0.08960
     0.13165 0.00553 0.00053 0.01443 0.00540
     0.08137 0.05964 0.04829 0.05559 0.04122
     0.00518 0.18653 0.22602 0.07208 0.07697
     0.37427 0.02005 0.02432 0.12954 0.00968
     0.01409 0.05069 0.19474 0.15708 0.01842
     0.09659 0.09836 0.10813 0.29945 0.00568))
  (define word-prior-vector (vector-immutable 0.023754 0.277952 0.698294))
  (define z-vector (vector-immutable 1 0 2 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 ))
  (define w-vector
    (vector-immutable
     40 0 14 0 55 0 68 0 50 0 67 0 44 0 31 0
     89 0 66 0 29 0 43 0 57 0 69 0 75 0 77
     0 44 0 39 0 40 0 97 0 53 0 96 0 43 0
     26 0 36 0 8 0 78 0 94 0 82 0 8 0 77 0
     55 0 7 0 86 0 42 0 95 0 10 0 94 0 22
     0 73 0 30 0 88 0 14 0 90 0 18 0 47 0
     52 0 26 0 30 0 76 0 74 0 92 0 20 0 63
     0 50 0 74 0 51 0 81 0 76 0 60 0 ))
  (define doc-vector
    (vector-immutable
     0 0 0 0 0 0 0 0 0 0 0 0 0 0
     0 0 0 0 0 0 0 0 0 0 0 0 0 0
     0 0 0 0 0 0 0 0 0 0 0 1 0 1
     0 1 0 1 0 1 0 1 0 1 0 1 0 1
     0 1 0 1 0 1 0 1 0 1 0 1 0 1
     0 1 0 1 0 1 0 1 0 2 0 2 0 2
     0 2 0 2 0 2 0 2 0 2 0 2 0 2
     0 2 0 2 0 2 0 2 0 2 0 2 0 2
     0 2 0 2 0 2 0 0))
  (define doc-update 86)
  (define nbgo-src (read-file "../hkr/nb_simp.hkr"))
  (define nbgo-mod-jit
    (debug-program nbgo-src
                   (passes
                    (list topic-prior-vector
                          word-prior-vector
                          z-vector
                          w-vector
                          doc-vector
                          doc-update))))
  ;; (jit-dump-module nbgo-mod-jit)

  (define nbgo-main (jit-get-function 'main nbgo-mod-jit))
  (define prob-type (jit-get-racket-type (env-lookup 'prob nbgo-mod-jit)))
  (define nat-type (jit-get-racket-type (env-lookup 'nat nbgo-mod-jit)))
  (define make-array-prob (jit-get-function 'make-array-prob nbgo-mod-jit))
  (define make-array-nat (jit-get-function 'make-array-nat nbgo-mod-jit))


  (define topic-prior
    (make-array-prob (vector-length topic-prior-vector) (vector->cblock topic-prior-vector prob-type)))
  (define word-prior
    (make-array-prob (vector-length word-prior-vector) (vector->cblock word-prior-vector prob-type)))
  (define z
    (make-array-nat (vector-length z-vector) (vector->cblock z-vector nat-type)))
  (define w
    (make-array-nat (vector-length w-vector) (vector->cblock w-vector nat-type)))
  (define doc
    (make-array-nat (vector-length doc-vector) (vector->cblock doc-vector nat-type)))
  ;; (define f (get-nbg-f 'f))
  (define result_raw (time (nbgo-main topic-prior word-prior z w doc doc-update)))
  (define get-array-prob (jit-get-function 'get-array-prob nbgo-mod-jit))

  (define result (cblock->vector (get-array-prob result_raw) prob-type (vector-length doc-vector)))
  (pretty-display result))

;; [1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0]
