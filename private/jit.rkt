#lang racket

(require sham/jit
         sham/ast)

(require "reduce-curry.rkt"
         "parse-sexp.rkt"
         "flatten.rkt"
         "expand-lc.rkt"
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

        (cons mbind->let         pp-expr)
        (cons remove-array-literals pp-expr)
        (cons flatten-anf        pp-expr)

        (cons combine-loops      pp-expr)

        (cons remove-unit-lets   pp-expr)
        (cons simplify-lets      pp-expr)

        (cons remove-empty-lets  pp-expr)
        (cons remove-unused-lets pp-expr)
        (cons remove-pairs       pp-expr)
        (cons simplify-lets      pp-expr)
        ;(cons folds->for         pp-expr)
        (cons to-stmt            pp-expr)

        (cons simplify-set       pp-expr)
        ;(cons remove-if-expr     pp-expr)
        (cons cleanup            pp-expr)
        (cons expand-to-lc       (compose pretty-display sham-ast->sexp))))



(define to-print? (make-parameter (const #t)))
(define to-not-print? (make-parameter (const #f)))

(define (debug-print compiler printer new-p)
  (when (debug-pass)
    (define pass-name (object-name compiler))
    (printf "applying pass: ~a\n" pass-name)
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
  (jit-verify-module module-env)
  (initialize-jit module-env #:opt-level 3))

(define (compile-file fname)
  (define src (read-file fname))
  (compile-src src))

(define (debug-src src)
  (parameterize ([debug-pass #t]
                 [to-print? (curryr member '(cleanup expand-to-lc))]
                 [to-not-print? (const #t)])
    (compile-src src)))
(define debug-file  (compose debug-src read-file))

(module+ test
;  (debug-file "../../testcode/hkrkt/clinicalTrial_simp.hkr")) ;;fix prelude ordering
;  (debug-file "../../testcode/hkrkt/linearRegression_simp.hkr")) ;;same ^^
;  (debug-file "../../testcode/hkrkt/gmm_gibbs_simp.hkr")) ;;add support for all array rators
  (debug-file "../../testcode/hkrkt/naive_bayes_gibbs_simp.hkr")) ;;needs some signed int and some other basic math functions
;  (debug-file "../../testcode/hkrkt/lda_gibbs_simp.hkr")) ;;error in combine loops
