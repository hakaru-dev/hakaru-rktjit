#lang racket
(require ffi/unsafe
         rackunit
         racket/runtime-path)

(require "../../jit.rkt"
         sham
         "utils.rkt"
         "../../utils.rkt")

(define vs-topics (list 2 1 0 2 1 0 0))
(define vs-words (list 0 3 3 2 1 2 0 1 2 3 3 0 0 3 2 1 0))
(define vs-docs (list 0 0 1 1 1 2 2 2 3 4 4 5 5 5 6 6 6))

(define full-words (map string->number (file->lines "./news/words")))
(define full-docs (map string->number (file->lines "./news/docs")))
(define full-topics (map string->number (file->lines "./news/topics")))

(define empty-nbinfo (list '() '() '() '() '() '()))

(define (create-test module-env topics words docs output-type)
  (define prog (jit-get-function 'prog module-env))
  (define init-rng (jit-get-function 'init-rng module-env))
  (init-rng)
  (printf "compiled\n")
  (define num-topics (add1 (argmax identity topics)))
  (define num-words (add1 (argmax identity words)))
  (define num-docs (add1 (last docs)))

  (define topic-prior
    (rkt->jit module-env '(array prob) (build-list num-topics (const 1.0))))
  (define word-prior
    (rkt->jit module-env '(array prob) (build-list num-words (const 1.0))))

  (define c-words (rkt->jit module-env '(array nat) words))
  (define c-docs (rkt->jit module-env '(array nat) docs))
  (define zs (rkt->jit module-env '(array nat) topics))
  (lambda (doc)
    (printf "calling prog\n")
    (define init-time (get-time))
    (define output-c (prog topic-prior word-prior zs c-words c-docs doc))
    (define el-time (elasp-time init-time))

    (printf "output from c: ~a\n" output-c)
    (define jit-out (jit->rkt module-env output-type output-c))
    (printf "time taken: ~a\n" el-time)
    (printf "output: ~a\n" jit-out)
    output-c))

(define penv (compile-file "./partial.hkr" empty-nbinfo))
(define run-full-test
  (create-test penv full-topics full-words full-docs '(array real)))

(module+ test
  (define our-out (run-full-test 9900))
  (printf "output: ~a\n" our-out)

  ;; (map (curryr check-= 0.0000000001) our-out
  ;;      (list -8659.371903184961 -9473.64535560285 -10152.497937672506 -9648.673918539233 -9606.47160672382 -9624.352183391884 -9468.15394886717
  ;;            -9580.842985576339 -9559.369326621854 -9634.407671463574 -9701.445215668664 -9415.152206831734 -9512.773749149055 -9368.555288242434
  ;;            -9336.744521041659 -9013.401662454573 -9349.721224754017 -9294.489616315372 -9260.335989131132 -8977.788497207463))
  )
;; our output
;; (list -8659.371903184969 -9473.645355602857 -10152.497937672513 -9648.673918539253 -9606.471606723844 -9624.352183391919 -9468.153948867177
;;       -9580.84298557636 -9559.369326621867 -9634.407671463578 -9701.445215668668 -9415.152206831754 -9512.77374914908 -9368.555288242453
;;       -9336.744521041664 -9013.401662454584 -9349.721224754028 -9294.489616315383 -9260.335989131163 -8977.788497207486)

;; output for 9900
;; (-458.1908976216084 -461.9810000474454 -478.57190842283467 -446.5395681936905 -453.6747904210667 -463.4369314698437 -450.8619961395306 -453.16976113041795 -445.12265609048325 -374.5512758505581 -458.091812993318 -447.1539476558063 -454.3902748158387 -450.796948060257 -448.8193584047232 -450.2865595520607 -450.52765701408424 -460.69817293449074 -453.0247297820897 -457.1166709136238)
