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
   initial-simplifications  ;; debug-print stop
   flatten-anf              ;; debug-print stop
   later-simplifications
   middle-simplifications   ;; debug-print
   later-simplifications    ;; debug-print stop
   fix-loop-lets            ;; debug-print
   combine-loops            ;; debug-print stop
   later-simplifications    ;; debug-print stop
   remove-pairs             debug-print stop

   pull-indexes             ;; debug-print

   later-simplifications    ;; debug-print stop


   to-stmt                  debug-print stop
   to-sham-lc               debug-print stop
   compile-with-sham
   optimize&init-jit))

(define (run-pipeline src arg-info)
  (define init-state
    (state src
           (make-immutable-hash (list (cons prog-arg-info arg-info)))
           passes))
  (define-values (env info) (run-state init-state))
  env)

;; this arginfo for now only talks about the prog function,
;; as for hakaru we only take one function
;; arginfo is a list of same size as args
;; the position in list maps to the info about
;; the argument of the function

;; each info is again list
;; which can have following:
;; (constant <value>)
;; (pair-info <ainfo> <binfo>)
;; (array-info (size <value>) (valuerange (from . to))
;; (curryhere)

(define (compile-src src arg-info)
  (run-pipeline src arg-info))

(define (compile-file fname arg-info)
  (compile-src (file->value fname) arg-info))

(define (debug-file fname arg-info)
  (parameterize ([hakrit-print-debug #t]
                 [debug-curry #f]
                 [debug-flatten-anf #f]
                 [debug-combine-loops #f]
                 [debug-to-sham #t])
    (compile-src (file->value fname) arg-info)))

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
      (debug-file "../../testcode/hkrkt/ClinicalTrial.hkr" ectinfo))
    (define f (jit-get-function 'prog ct-module-env))
    (dv ct-module-env))
  (define (dolr)
    (define nlr 10)
    (define lrinfo (list
                    (list `(arrayinfo . ((size . ,nlr))))
                    (list `(arrayinfo . ((size . ,nlr))))))
    (define lr-module-env
      (debug-file "../../testcode/hkrkt/LinearRegression.hkr" lrinfo))
    (define f (jit-get-function 'prog lr-module-env))
    (dv lr-module-env))
  (define (dogg)
    (define classes 3)
    (define points 10)
    (define gmminfo
      (list
       (list )
       (list `(array-info . ((size . ,classes)
                            (elem-info . ((prob-info . ((constant . 0)))))
                            (constant . #t)))
             ;; `(attrs . (remove))
             )
       (list `(array-info
               . ((size . ,points)
                  (elem-info
                   . ((nat-info
                       . ((value-range . (0 . ,(- classes 1)))))))))
             ;; `(attrs . (movedown))
             )
       (list `(array-info . ((size . ,points))))
       ;             `(fninfo . (curry)))

       (list `(nat-info . ((value-range . (0 . ,(- points 1))))))))
    (define gg-module-env
      (debug-file "../../testcode/hkrkt/GmmGibbs.hkr" gmminfo))
    (jit-dump-function gg-module-env 'prog)
    (jit-verify-module gg-module-env))
  ;; (dogg)
  (define (donb num-topics num-words num-docs words-size)
    (define empty-info (list '() '() '() '() '() '()))
    (define full-info
      (list
       (list `(array-info . ((size . ,num-topics)
                             (elem-info . ((prob-info . ((constant . 0)))))
                             (constant . #t)))
             `(attrs . (remove)))
       (list `(array-info . ((size . ,num-words)
                             (elem-info . ((prob-info . ((constant . 0)))))
                             (constant . #t)))
             `(attrs . (remove)))
       (list `(array-info . ((size . ,num-docs)
                             (elem-info . ((nat-info
                                            . ((value-range
                                                . (0 . ,(- num-topics 1))))))))))
       (list `(array-info . ((size . ,words-size)
                             (elem-info . ((nat-info
                                            . ((value-range
                                                . (0 . ,(- num-words 1)))))))))
             `(attrs . (no-change)))
       (list `(array-info . ((size . ,words-size)
                             (elem-info . ((nat-info
                                            . ((value-range
                                                . (0 . ,(- num-docs 1)))))))))
             `(attrs . (no-change)))
       (list `(nat-info . ((value-range . (0 . ,(- num-docs 1))))))))

    (define nb-module-env
      (debug-file "../../testcode/hkrkt/NaiveBayesGibbs.hkr" full-info))
    (jit-dump-function nb-module-env 'prog)
    (optimize-module nb-module-env)
    ;; (basic-optimize-module nb-module-env #:opt-level 1)
    (jit-dump-function nb-module-env 'prog)

    ;; (jit-verify-module nb-module-env)
    ;; (jit-write-module nb-module-env "nb.ll")
    ;; (jit-get-function 'prog nb-module-env)
    (disassemble-ffi-function (jit-get-function-ptr 'prog nb-module-env)
                              #:size 1000)
    )

  ;; (dogg)
  (donb 20 59967 19997 2435579)
  ;; (define zp-info (list '()))
  ;; (define zp-env (compile-file "../test/zero-product.hkr" zp-info ))
  ;; (define real2prob (jit-get-function (string->symbol "real2prob") zp-env))
  ;; (define prob2real (jit-get-function (string->symbol "prob2real") zp-env))
  ;; (define zp-prog (jit-get-function 'prog zp-env))
  ;; (prob2real (zp-prog 0.0))

  )
