#lang racket

(require sham/jit
         sham/ast)

(require "reduce-curry.rkt"
         "parse-sexp.rkt"
         "flatten.rkt"
         "expand-lc.rkt"
         "basic-fluff.rkt"
         "ast.rkt"
         "utils.rkt"
         "simplifications.rkt"
         "to-stmt.rkt"
         "removals.rkt"
         "do-bucket.rkt")

(provide (all-defined-out))

(define (read-file filename)
  (file->value filename))

(define pp-expr (compose pretty-display print-expr))
(define stop (cons (λ (e) (error 'stop)) pp-expr))
(define passes
  (list (cons reduce-curry       pretty-display)
        (cons parse-sexp         pp-expr)

        (cons macro-functions    pp-expr)
        (cons simplify-match     pp-expr) 
        ;stop
        (cons mbind->let         pp-expr)
        (cons remove-array-literals pp-expr)
        (cons flatten-anf        pp-expr)
        ;stop
        (cons combine-loops      pp-expr)
        ;stop
        (cons bucket->for        pp-expr)
        (cons remove-unit-lets   pp-expr)
        (cons simplify-lets      pp-expr)

        (cons remove-empty-lets  pp-expr)
        (cons remove-unused-lets pp-expr)
        (cons remove-pairs       pp-expr)

        (cons folds->for         pp-expr)
        (cons to-stmt            pp-expr)
        (cons simplify-set       pp-expr)
        (cons remove-if-expr     pp-expr)
;        stop
        (cons expand-to-lc       (compose pretty-display sham-ast->sexp))
        stop
        (cons add-fluff          (compose pretty-display print-sham-ast))))

(define (compile-src src)
  (define prog-module
   (for/fold ([prg src])
             ([p passes])
     (define compiler (car p))
     (define new-p (compiler prg))
     (when (debug-pass)
       (define printer (cdr p))
       (printf "applying ~a\n" (object-name compiler))
       (parameterize ([pretty-print-current-style-table
                       (pretty-print-extend-style-table
                        (pretty-print-current-style-table)
                        '(block define-variables define-function assign while function)
                        '(begin let lambda set! do mbind))]
                      [pretty-print-columns 100])
         (printer new-p)))
     new-p))
  (define module-env (compile-module prog-module))
  (jit-optimize-module module-env #:opt-level 3)
  (jit-optimize-function module-env #:opt-level 3)
  (jit-verify-module module-env)
  (initialize-jit module-env #:opt-level 3))
(define (compile-file fname)
  (define src (read-file fname))
  (compile-src src))

(define (debug-src src)
  (parameterize ([debug-pass #t])
    (compile-src src)))
(define debug-file  (compose debug-src read-file))
(module+ test
  (debug-file "../../testcode/hkrkt/clinicalTrial_simp.hkr"))
;  (debug-file "../../testcode/hkrkt/naive_bayes_gibbs_simp.hkr"))
;; (debug-file "../hkr/nb_simpbucket.hkr")
;; (module+ test
;;   (require ffi/unsafe)
;;   (require "jit-utils.rkt")
;;   ;; (define nbgo-src (read-file "../hkr/nb_simp.hkr"))
;;    (define mod (debug-file "../hkr/nb_simpbucket.hkr"))
;;   ;; (define nbgo-src (read-file "../hkr/easyroad.hkr"))
;;   ;; (define nbgo-src (read-file "../hkr/easyroad_d_s.hkr"))
;;   ;; (define nbgo-src (read-file "../test/unit/bucket-nb.hkr"))
;;   ;; (define nbgo-src (read-file "../test/unit/bucket-index.hkr"))
;;   ;; (define nbgo-mod-jit
;;   ;;   (debug-src nbgo-src))

;;   (jit-dump-module nbgo-mod-jit)
;;   ;; (jit-write-bitcode nbgo-mod-jit "nb_simpbucket.bc")
;;   (jit-write-module nbgo-mod-jit "nb_simpbucket.ll")
;;   (define main (jit-get-function 'main nbgo-mod-jit))
;;   (define init-rng (jit-get-function 'init-rng nbgo-mod-jit))
;;   (init-rng)
;;   ;; (printf "f: ~a" (env-lookup 'main nbgo-mod-jit))
;;   ;; (printf "easyroad: ~a" (main))
;;   ;; (error 'stop)
;;   (hakaru-defines nbgo-mod-jit)

;;   ;; (define nat-array (make-c-array-nat (replicate-vector 100 1)))
;;   ;; (define raw (main nat-array))
;;   ;; (pretty-display (cblock->vector (get-array-nat raw) nat-type (size-array-nat raw)))



;;   (define read-from-csv (compose make-c-array-nat read-vector-from-csv))
;;   (define topic-prior (make-c-array-prob (replicate-vector 20 1.0))) ;big 20  ;small 10
;;   (define word-prior (make-c-array-prob (replicate-vector 7022 1.0))) ;big 7022;small 100
;;   (define v (read-from-csv "../test/input/big-arg3.csv")) ;;size 40x0   ;; values 0-20
;;   (define words (read-from-csv "../test/input/big-arg4.csv")) ;;size 47049 ;; values 0-7022
;;   (define docs (read-from-csv "../test/input/big-arg5.csv")) ;;size 47049 ;; values 0-400
;;   (define docUpdate 0) ;; value 0-400
;;   (define result-raw (time (main topic-prior word-prior v words docs docUpdate)))
;;   (define result-vector
;;     (cblock->vector (get-array-prob result-raw) prob-type (size-array-prob result-raw)))
;;   (pretty-display result-vector)


;;   ;;vectorized
;;   (require sham/private/types
;;            sham/private/env)
;;   (define flib (ffi-lib "nbs.so"))
;;   (define fref (env-lookup 'main nbgo-mod-jit))
;;   (define f-type (internal-type-racket (env-type-prim (env-jit-function-type fref))))
;;   (define nmain (get-ffi-obj "main" flib f-type))
;;   (define nresult-raw (time (nmain topic-prior word-prior v words docs docUpdate)))
;;   (define nresult-vector
;;     (cblock->vector (get-array-prob result-raw) prob-type (size-array-prob nresult-raw)))
;;   (pretty-display nresult-vector)
;;   )



