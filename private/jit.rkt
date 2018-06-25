#lang racket

(require sham
         "ast.rkt"
         "pass.rkt"
         "pass/utils.rkt"
         "parse-hk.rkt"
         "utils.rkt")

(provide compile-file
         debug-file
         compile-function
         get-function)

(define basic-pass-list
  (list
   initial-simplifications     ;; debug-print stop
   flatten-anf              debug-print ;; stop
   later-simplifications    debug-print ;; stop
   middle-simplifications   ;; debug-print
   later-simplifications    ;; debug-print stop
   fix-loop-lets             debug-print
   combine-loops            debug-print ;; stop
   later-simplifications     debug-print stop
   remove-pairs             ;; debug-print stop

   pull-indexes             ;; debug-print

   later-simplifications  debug-print stop

   to-stmt                  ;; debug-print ;;stop
   compile-opts
   to-sham-lc               debug-print ;; stop
   compile-with-sham
   optimize&init-jit))
(define passes
  `(,clean-curry
    ,parse-sexp ;; ,debug-print ;; stop
    ,@basic-pass-list))

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

  (run-pipeline src arg-info)
)

(define (compile-file fname arg-info)
  (compile-src (file->value fname) arg-info))

(define (get-function sym env)
  (jit-get-function sym env))

(define (compile-function prog-expr prog-info)
  (define-values (env info)
    (run-state
     (state
      (list prog-expr)
      (make-immutable-hash (list (cons prog-arg-info prog-info)))
      basic-pass-list)))
  env)

(define (debug-file fname arg-info)
  (parameterize ([hakrit-print-debug #f]
                 [debug-curry #f]
                 [debug-flatten-anf #f]
                 [debug-combine-loops #t]
                 [debug-later-simplifications #f]
                 [debug-to-sham #f]
                 [debug-print-stop #t])
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
    (define empty-info '(() () () () ()))
    (define full-info
      `((;; (attrs . (constant))
         )
        ((array-info . ((size . ,classes)
                        (elem-info . ((prob-info . ((constant . 0.0)))))
                        ))
         (attrs . (constant)))
        ((array-info
          . ((size . ,points)
             (elem-info
              . ((nat-info
                  . ((value-range . (0 . ,(- classes 1))))))))))
        ((array-info . ((size . ,points)))
         (attrs . (constant))
         (value .  [3.8728103253204136
                    1.1452918218810444
                    -0.37443733246614497
                    2.2524280674567634
                    0.1088871787126991
                    2.2484645323958334
                    0.19013878436498044
                    1.4032911741452248
                    2.1930977191694936
                    1.7312282946567383]))
        ((nat-info . ((value-range . (0 . ,(- points 1))))))))

    (define gg-module-env
      (debug-file "../../testcode/hkrkt/GmmGibbs.hkr" full-info))
    (jit-dump-function gg-module-env 'prog)
    (jit-verify-module gg-module-env)
    (define prog (jit-get-function 'prog gg-module-env))
    (define real2prob (jit-get-function 'real2prob gg-module-env))
    (define prob2real (jit-get-function 'prob2real gg-module-env))

    (define stdev (real2prob 14.0))
    (define zs (list->cblock `[2 2 2 2 1 2 2 2 2 2] _uint64))
    (define as (list->cblock (list 0.0 0.0 0.0) _double))
    (define doc 0)

    (printf "calling prog:\n")
    (define output-c (prog stdev ;; as
                           zs doc))
    (printf "output-c: ~a" output-c)
    ;; (define output-list (map prob2real (cblock->list output-c _double 3)))
    ;; output-list
    )
  ;; (dogg)



  (define (donb num-topics num-words num-docs words-size)
    (define empty-info (list '() '() '() '() '() '()))
    (define full-info
      `(((array-info . ((size . ,num-topics)
                        (elem-info . ((prob-info . ((constant . 0)))))))
         (attrs . (constant)))
        ((array-info . ((size . ,num-words)
                        (elem-info . ((prob-info . ((constant . 0)))))))
         (attrs . (constant)))
        ((array-info . ((size . ,num-docs)
                        (elem-info . ((nat-info
                                       . ((value-range
                                           . (0 . ,(- num-topics 1))))))))))
        ((array-info . ((size . ,words-size)
                        (elem-info . ((nat-info
                                       . ((value-range
                                           . (0 . ,(- num-words 1)))))))))
         (value . (0))
         (attrs . (constant)))
        ((array-info . ((size . ,words-size)
                        (elem-info . ((nat-info
                                       . ((value-range
                                           . (0 . ,(- num-docs 1)))))))))
         (value . (0))
         (attrs . (constant)))
        ((nat-info . ((value-range . (0 . ,(- num-docs 1))))))))

    (define nb-module-env
      (debug-file "../../testcode/hkrkt/NaiveBayesGibbs.hkr" full-info))

    ;; (jit-dump-module nb-module-env)
    ;; (jit-dump-function nb-module-env 'prog)
    ;; (jit-write-module nb-module-env "nb.ll")
    (jit-get-function 'prog nb-module-env)
    ;; (disassemble-ffi-function (jit-get-function-ptr 'prog nb-module-env)
    ;;                           #:size 1000)
    )

  (define (do-lda num-topics num-words num-docs words-size)
    (define empty-info (list '() '() '() '() '() '() '()))
    (define full-info
      `(((array-info . ((size . ,num-topics))))
        ((array-info . ((size . ,num-words))))
        ()
        ;; ((nat-info . ((value . ,num-docs))))
        ((array-info . ((size . ,words-size)
                        (value . (0))
                        )))
        ((array-info . ((size . ,words-size)
                        (value . (0))
                        )))
        ((array-info . ((size . ,words-size))))
        ((nat-info . ((value-range . (0 . ,(- num-words 1))))))))
    (define nb-module-env
      (debug-file "../../testcode/hkrkt/LdaGibbs.hkr" full-info))

    ;; (jit-dump-module nb-module-env)
    ;; (jit-dump-function nb-module-env 'prog)
    ;; (jit-write-module nb-module-env "nb.ll")
    (jit-get-function 'prog nb-module-env)
    ;; (disassemble-ffi-function (jit-get-function-ptr 'prog nb-module-env)
    ;;                           #:size 1000)
    )
  ;; (dogg)
  (do-lda 20 59967 19997 2435579)
  ;; (donb 20 59967 19997 2435579)
  )
