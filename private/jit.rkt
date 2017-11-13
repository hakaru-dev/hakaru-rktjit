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

   pause
   stop

   parse-sexp
   initial-simplifications
   flatten-anf
   combine-loops
   later-simplifications
   pull-indexes
   later-simplifications
   to-stmt
   to-sham-lc
   compile-with-sham
   optimize&init-jit))

(define (run-pipeline src arg-info)
  (define init-state
    (state src (make-immutable-hash (cons prog-arg-info (list->vector arg-info))) passes))
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
  (define final-state (run-pipeline src arg-info))
  (state-prg final-state))

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
      (jit-verify-module mod-env)))
  (define (doct)
    (define nct 10)
    (define ctinfo (list (list `(constant ,nct))
                         (list `(pairinfo ((arrayinfo (size . ,nct)
                                                      (valuerange (0 . 1))))
                                          ((arrayinfo (size . ,nct)
                                                      (valuerange (0 . 1))))))))
    (define ct-module-env
      (compile-file "../../testcode/hkrkt/ClinicalTrial.hkr" ctinfo))
    (dv ct-module-env))


  (define (dolr)
    (define nlr 10)
    (define lrinfo (list
                    (list `(arrayinfo (size . ,nlr)) 'curryhere)
                    (list `(arrayinfo (size . ,nlr)))))
    (define lr-module-env (compile-file "../../testcode/hkrkt/LinearRegression.hkr" lrinfo))
    (dv lr-module-env))

  (define (dogg)
    (define classes 3)
    (define points 10)
    (define gmminfo (list
                     (list `(arrayinfo (size . ,classes) (valuerange (1 . 1))))
                     (list `(arrayinfo (size . ,points) (valuerange (0 . ,(- classes 1)))))
                     (list `(arrayinfo (size . ,points)) 'curryhere)
                     (list `(natinfo (valuerange (0 . ,(- points 1)))))))
    (define gg-module-env(compile-file "../../testcode/hkrkt/GmmGibbs.hkr" gmminfo))
    (dv gg-module-env))



  (doct))
