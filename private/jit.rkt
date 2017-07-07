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
(require "simplifications.rkt")
(require "removals.rkt")
(require "do-bucket.rkt")
(provide (all-defined-out))

(define (read-file filename)
  (call-with-input-file filename
    (lambda (in)
      (read in))))
(define pp-expr (compose pretty-display print-expr))
(define stop (cons (λ (e) (error 'stop)) pp-expr))
(define passes
  (list (cons reduce-curry pretty-display)
        (cons parse-sexp         pp-expr)

        (cons simplify-match     pp-expr)
        (cons flatten-anf        pp-expr)

        (cons bucket->for pp-expr)
;        stop
        (cons remove-unit-lets   pp-expr)

        (cons simplify-lets pp-expr)
        (cons remove-empty-lets  pp-expr)
        (cons remove-unused-lets pp-expr)
        (cons remove-pairs pp-expr)

        (cons flatten-to-stmt pp-expr)

        (cons expand-to-lc (compose  pretty-display print-ast))
        (cons add-fluff pretty-display)))

(define (debug-program prg cmplrs)
  (define prog-ast
   (for/fold ([prg prg])
             ([c cmplrs])
     (define compiler (car c))
     (define printer (cdr c))
     (printf "\n\napplying ~a\n" (object-name compiler))
     (let ([p (compiler prg)])
       (unless (member (object-name compiler)
                       '(;reduce-curry
                         ;parse-exp flatten-anf
                         ;simplify-match
                         ;expand-to-lc
                         ;add-fluff
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
  ;; (jit-dump-module module-env)
  ;; (jit-dump-function module-env 'main)
  (define mod-env (initialize-jit module-env #:opt-level 3))
  mod-env)

(define debug-pass (make-parameter #f))

(define (compile-src src)
  (define prog-module
   (for/fold ([prg src])
             ([p passes])
     (define compiler (car p))
     (define printer (cdr p))
     (printf "\n\napplying ~a\n" (object-name compiler))
     (compiler prg)))
  (define module-env (compile-module prog-module))
  (jit-optimize-module module-env #:opt-level 3)
  (initialize-jit module-env #:opt-level 3))

(module+ test
  (require ffi/unsafe)
  (require "jit-utils.rkt")
  (define nbgo-src (read-file "../hkr/nb_simp.hkr"))
;  (define nbgo-src (read-file "../test/unit/bucket-nb.hkr"))
;  (define nbgo-src (read-file "../test/unit/bucket-index.hkr"))
  (define nbgo-mod-jit
    (compile-src nbgo-src)
    ;; (debug-program nbgo-src
    ;;                passes)
    )
  ;; (jit-dump-module nbgo-mod-jit)
  ;; (jit-write-bitcode nbgo-mod-jit "test.bc")
  (define main (jit-get-function 'main nbgo-mod-jit))
  (hakaru-defines nbgo-mod-jit)

  ;; (define nat-array (make-c-array-nat (replicate-vector 100 1)))
  ;; (define raw (main nat-array))
  ;; (pretty-display (cblock->vector (get-array-nat raw) nat-type (size-array-nat raw)))
  ;; (error 'stop)


  (define read-from-csv (compose make-c-array-nat read-vector-from-csv))
  (define topic-prior (make-c-array-prob (replicate-vector 10 1.0)))
  (define word-prior (make-c-array-prob (replicate-vector 100 1.0)))
  (define v (read-from-csv "../test/input/small-arg3.csv")) ;;size 40x0   ;; values 0-20
  (define words (read-from-csv "../test/input/small-arg4.csv")) ;;size 47049 ;; values 0-7022
  (define docs (read-from-csv "../test/input/small-arg5.csv")) ;;size 47049 ;; values 0-400
  (define docUpdate 0) ;; value 0-400
  (define result-raw (time (main topic-prior word-prior v words docs docUpdate)))
  (define result-vector
    (cblock->vector (get-array-prob result-raw) prob-type (size-array-prob result-raw)))
  (pretty-display result-vector)
  )
