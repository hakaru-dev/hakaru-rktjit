#lang racket

(require sham/jit
         sham/ast
         "ast.rkt"
         "pass.rkt"
         "utils.rkt")

(provide (all-defined-out))



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
  ;; (jit-optimize-module module-env #:opt-level 3)
  ;; (jit-optimize-function module-env #:opt-level 3)
  ;; (jit-dump-module module-env)
  ;; (jit-dump-function module-env 'prog)

  (jit-verify-module module-env)
  (initialize-jit module-env #:opt-level 3))

(define compile-file (compose compile-src file->value))

(define (debug-src src)
  (parameterize
      ([debug-pass #t]
       [to-print?
        (curryr
         member
         '())];reduce-curry
           ;parse-sexp
           ;; initial-simplifications
           ;; flatten-anf
           ;; combine-loops
           ;; later-simplifications
           ;; to-stmt
           ;; expand-to-lc))]
       [to-not-print? (const #t)])
    (compile-src src)))

(define debug-file (compose debug-src file->value))

(define (debug-store-file src-fname out-fname)
  (call-with-output-file out-fname
    (λ (out-port)
      (parameterize ([current-output-port out-port])
        (debug-file src-fname)))
    #:exists 'truncate/replace))

(module+ test
  (debug-file "../../testcode/hkrkt/clinicalTrial_simp.hkr")
  (debug-file "../../testcode/hkrkt/linearRegression_simp.hkr")
  (debug-file "../../testcode/hkrkt/gmm_gibbs_simp.hkr")
  (debug-file "../../testcode/hkrkt/naive_bayes_gibbs_simp.hkr")
  (debug-file "../../testcode/hkrkt/lda_gibbs_simp.hkr"))

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
