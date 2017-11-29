#lang racket
(require ffi/unsafe
         rackunit
         racket/runtime-path)

(require "../../jit.rkt"
         sham
         "utils.rkt")

(define vs-topics (list 2 1 0 2 1 0 0))
(define vs-words (list 0 3 3 2 1 2 0 1 2 3 3 0 0 3 2 1 0))
(define vs-docs (list 0 0 1 1 1 2 2 2 3 4 4 5 5 5 6 6 6))

(define full-words (map string->number (file->lines "./news/words")))
(define full-docs (map string->number (file->lines "./news/docs")))
(define full-topics (map string->number (file->lines "./news/topics")))

(define empty-nbinfo (list '() '() '() '() '() '()))

(define (run-test module-env topics words docs output-type output-hs)

  ;; (printf "running naive bayes:\n\t topics: ~a\n\t words: ~a\n\t docs: ~a\n" topics words docs)

  (printf "compiled\n")
  (define prog (jit-get-function 'prog module-env))
  (define init-rng (jit-get-function 'init-rng module-env))
  (init-rng)
  (define num-topics (add1 (argmax identity topics)))
  (define num-words (add1 (argmax identity words)))
  (define num-docs (add1 (last docs)))

  (define topic-prior (rkt->jit module-env '(array prob) (build-list num-topics (const 1.0))) )
  (define word-prior (rkt->jit module-env '(array prob) (build-list num-words (const 1.0))))

  (define c-words (rkt->jit module-env '(array nat) words))
  (define c-docs (rkt->jit module-env '(array nat) docs))
  (define zs (rkt->jit module-env '(array nat) topics))
  (define doc 0)



  ;; (printf "words: ~a\n"  words)
  ;; (for ([i (get-size-nat-array c-words)])
  ;;   (printf "~a\n" (get-index-nat-array c-words i)))
  ;; (printf "\n")
  ;; (printf "docs: ~a\n"  docs)
  ;; (for ([i (get-size-nat-array c-docs)])
  ;;   (printf "~a\n" (get-index-nat-array c-docs i)))
  ;; (printf "\n")

  ;; (printf "zs: ~a\n"  topics)
  ;; (for ([i (get-size-nat-array zs)])
  ;;   (printf "~a\n" (get-index-nat-array zs i)))
  ;; (printf "\n")

  (printf "done making arrays, calling prog\n")
  (define output-c (prog topic-prior word-prior zs c-words c-docs doc ))

  ;; (map (curryr check-= 0.00001) (jit->rkt module-env output-type output-c) output-hs)
  (printf "output from prog: ~a\n" (jit->rkt module-env output-type output-c))
    (printf "output from hskl: ~a\n" output-hs))

(define (run-vs-test fname type hs-output)
  (printf "running test: ~a\n" fname)
  (run-test (compile-file fname empty-nbinfo)
            vs-topics vs-words vs-docs type hs-output))

(define (run-full-test fname type hs-output)
  (run-test (compile-file fname empty-nbinfo)
            full-topics full-words full-docs type hs-output))

;; (run-full-test "./NaiveBayesGibbs.hkr" 'nat 0)
(run-full-test "./partial.hkr" '(array prob) '()

;; our output
;; (-8659.371903184969 -9473.645355602857 -10152.497937672513 -9648.673918539253 -9606.471606723844 -9624.352183391919 -9468.153948867177 -9580.84298557636 -9559.369326621867 -9634.407671463578 -9701.445215668668 -9415.152206831754 -9512.77374914908 -9368.555288242453 -9336.744521041664 -9013.401662454584 -9349.721224754028 -9294.489616315383 -9260.335989131163 -8977.788497207486)


;;haskell output
;; [-8659.371903184961,-9473.64535560285,-10152.497937672506,-9648.673918539233,-9606.47160672382,-9624.352183391884,-9468.15394886717,-9580.842985576339,-9559.369326621854,-9634.407671463574,-9701.445215668664,-9415.152206831734,-9512.773749149055,-9368.555288242434,-9336.744521041659,-9013.401662454573,-9349.721224754017,-9294.489616315372,-9260.335989131132,-8977.788497207463] 
