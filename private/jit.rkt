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
   pull-indexes

   to-stmt
   to-sham-lc
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
      (jit-dump-function  mod-env 'categorical$array<3.prob>)

      (jit-verify-module mod-env))
    (void))

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
      (compile-file "../../testcode/hkrkt/ClinicalTrial.hkr" ectinfo))
    (define f (jit-get-function 'prog ct-module-env))
    (dv ct-module-env))


  (define (dolr)
    (define nlr 10)
    (define lrinfo (list
                    (list `(arrayinfo . ((size . ,nlr))) 'curry)
                    (list `(arrayinfo . ((size . ,nlr))))))
    (define lr-module-env
      (compile-file "../../testcode/hkrkt/LinearRegression.hkr" lrinfo))
    (define f (jit-get-function 'prog lr-module-env))
   (dv lr-module-env))

  (define (dogg)
    (define classes 3)
    (define points 10)
    (define gmminfo
      (list
       (list `(arrayinfo . ((size . ,classes)
                            (typeinfo . ((probinfo . ((constant . 0)))))
                            (constant . #t)))
             `(fninfo . (remove)))
       (list `(arrayinfo
               . ((size . ,points)
                  (typeinfo
                   . ((natinfo
                       . ((valuerange . (0 . ,(- classes 1)))))))))
             `(fninfo . (movedown curry)))
       (list `(arrayinfo . ((size . ,points))))
       (list `(natinfo . ((valuerange . (0 . ,(- points 1))))))))
    (define gg-module-env
      (compile-file "../../testcode/hkrkt/GmmGibbs.hkr" gmminfo))
    ;    (define f (jit-get-function 'prog gg-module-env))
    (dv gg-module-env))

  (define (donb)

    (define nbinfo (list
                    (list)
                    (list)
                    (list)
                    (list)
                    (list)
                    (list)))
    (define nb-module-env
      (compile-file "../../testcode/hkrkt/NaiveBayesGibbs.hkr" nbinfo))
    (dv nb-module-env))


;  (printf "pipeline ClinicalTrial\n")(doct))
;  (printf "\n\n\npipeline LinearRegression\n")  (dolr))
  (printf "\n\n\npipeline GmmGibbs\n")  (dogg))
