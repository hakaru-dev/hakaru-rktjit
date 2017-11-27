#lang racket

(require sham
         "ast.rkt"
         "pass.rkt"
         "pass/utils.rkt"
         "utils.rkt")

(provide compile-file)

(define passes

  (list
   clean-curry

   parse-sexp

   initial-simplifications
   flatten-anf
   combine-loops

   later-simplifications

   pull-indexes
   later-simplifications

   to-stmt
   debug-print

   to-sham-lc
   debug-print

   compile-with-sham
   optimize&init-jit))

(define (run-pipeline src arg-info)
  (define init-state
    (state src
           (make-immutable-hash (list (cons prog-arg-info arg-info)))
           passes))
  (run-state init-state))

;; this arginfo for now only talks about the prog function,
;; as for hakaru we only take one function
;; arginfo is a list of same size as args
;; the position in list maps to the info about
;; the argument of the function

;; each info is again list
;; which can have following:
;; (constant <value>)
;; (pairinfo <ainfo> <binfo>)
;; (arrayinfo (size <value>) (valuerange (from . to))
;; (curryhere)

(define (compile-src src arg-info)
  (run-pipeline src arg-info))

(define (compile-file fname arg-info)
  (compile-src (file->value fname) arg-info))

(define (debug-store-file src-fname out-fname)
  (call-with-output-file out-fname
    (λ (out-port)
      (parameterize ([current-output-port out-port])
        (compile-file src-fname)))
    #:exists 'truncate/replace))

(module+ test
  (require ffi/unsafe)
  (require disassemble)
  (define (dv mod-env)
    (when mod-env
      (jit-dump-module mod-env)
      (jit-verify-module mod-env))
    (void))
  (define (cf fname inf)
    (parameterize [(debug-curry #f)]
      (compile-file fname inf)))

  (define (doct)
    (define nct 1000)
    (define ctinfo
      (list (list `(natinfo . ((constant . ,nct))))
            (list `(pairinfo
                    . ((ainfo . ((arrayinfo . ((size . ,nct)))))
                       (binfo . ((arrayinfo . ((size . ,nct))))))))))
    (define ectinfo
      (list (list) (list)))

    (define ct-module-env
      (cf "../../testcode/hkrkt/ClinicalTrial.hkr" ectinfo))
    (define f (jit-get-function 'prog ct-module-env))
    (dv ct-module-env))
  (define (dolr)
    (define nlr 10)
    (define lrinfo (list
                    (list `(arrayinfo . ((size . ,nlr))))
                    (list `(arrayinfo . ((size . ,nlr))))))
    (define lr-module-env
      (cf "../../testcode/hkrkt/LinearRegression.hkr" lrinfo))
    (define f (jit-get-function 'prog lr-module-env))
    (dv lr-module-env))
  (define (donb num-topics num-words num-docs words-size docs-size)
    (define empty-info (list '() '() '() '() '() '()))
    (define full-info
    (list
     (list `(arrayinfo . ((size . ,num-topics)
                          (typeinfo . ((probinfo . ((constant . 0)))))
                          (constant . #t)))
           `(fninfo . (remove)))
     (list `(arrayinfo . ((size . ,num-words)
                          (typeinfo . ((probinfo . ((constant . 0)))))
                          (constant . #t)))
           `(fninfo . (remove)))
     (list `(arrayinfo . ((size . ,num-docs)
                          (typeinfo . ((probinfo . ((constant . 0)))))
                          (constant . #t))))
     (list `(arrayinfo . ((size . ,words-size))))
     (list `(arrayinfo . ((size . ,docs-size))))
     (list `(natinfo . ((valuerange . (0 . ,(- num-docs 1))))))))

    (define nb-module-env
      (cf "../../testcode/hkrkt/NaiveBayesGibbs.hkr" empty-info))
    (jit-dump-function nb-module-env 'prog)
    (jit-verify-module nb-module-env))
  (define (dogg)
    (define classes 3)
    (define points 10)
    (define gmminfo
      (list
       (list )
       (list `(arrayinfo . ((size . ,classes)
                            (typeinfo . ((probinfo . ((constant . 0)))))
                            (constant . #t)))
             `(fninfo . (remove)))
       (list `(arrayinfo
               . ((size . ,points)
                  (typeinfo
                   . ((natinfo
                       . ((valuerange . (0 . ,(- classes 1)))))))))
             `(fninfo . (movedown)))
       (list `(arrayinfo . ((size . ,points))))
       ;             `(fninfo . (curry)))

       (list `(natinfo . ((valuerange . (0 . ,(- points 1))))))))
    (define gg-module-env
      (cf "../../testcode/hkrkt/GmmGibbs.hkr" gmminfo))
    (jit-dump-function gg-module-env 'prog)
    (jit-verify-module gg-module-env))

  ;; (dogg)
  ;; (donb)
  (define zp-info (list '()))
  (define zp-env (compile-file "../test/zero-product.hkr" zp-info ))
  (define real2prob (jit-get-function (string->symbol "real2prob") zp-env))
  (define prob2real (jit-get-function (string->symbol "prob2real") zp-env))
  (define zp-prog (jit-get-function 'prog zp-env))
  (prob2real (zp-prog 0.0))

  )
