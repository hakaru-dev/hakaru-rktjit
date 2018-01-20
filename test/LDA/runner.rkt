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

(define penv (compile-file "./NaiveBayesGibbs.hkr" empty-nbinfo))
(define run-full-test
  (create-test penv full-topics full-words full-docs 'nat))

(module+ test
  (define our-out (run-full-test 19800))
  (printf "output: ~a\n" our-out))
