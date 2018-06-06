#lang racket
(require ffi/unsafe
         rackunit
         racket/runtime-path)

(require "../../jit.rkt"
         sham
         "utils.rkt"
         "../../utils.rkt")

(define words (map string->number (file->lines "../news/words")))
(define docs (map string->number (file->lines "../news/docs")))
(define topics (map string->number (file->lines "../news/topics")))

(define words-size (length words))
(define docs-size (length docs))
(define topics-size (length topics))

(define num-docs (add1 (last docs)))
(define num-words (add1 (argmax identity words)))
(define num-topics (add1 (argmax identity topics)))

(define full-info
  `(((array-info . ((size . ,num-topics))))
    ((array-info . ((size . ,num-words))))
    ((array-info . ((size . ,num-docs)
                    (elem-info . ((nat-info
                                   . ((value-range
                                       . (0 . ,(- num-topics 1))))))))))
    ((array-info . ((size . ,words-size)
                    (elem-info . ((nat-info
                                   . ((value-range
                                       . (0 . ,(- num-words 1)))))))
                    (value . ,words))))
    ((array-info . ((size . ,words-size)
                    (elem-info . ((nat-info
                                   . ((value-range
                                       . (0 . ,(- num-docs 1)))))))
                    (value . ,docs))))
    ((nat-info . ((value-range . (0 . ,(- num-docs 1))))))))

(define module-env (compile-file "./partial.hkr" full-info))
(define prog (jit-get-function 'prog module-env))
(define init-rng (jit-get-function 'init-rng module-env))
(init-rng)
(define set-index-nat-array (jit-get-function (string->symbol (format "set-index!$array<~a.~a>" num-topics 'nat)) module-env ))
(define get-index-nat-array (jit-get-function (string->symbol (format "get-index$array<~a.~a>" num-topics 'nat)) module-env ))
(define get-index-prob-array (jit-get-function (string->symbol (format "get-index$array<~a.~a>" num-topics 'prob)) module-env ))

(printf "compiled\n")

(define topic-prior (list->cblock (build-list num-topics (const 0.0)) _double))
(define word-prior (list->cblock (build-list num-words (const 0.0)) _double))

(define c-words (list->cblock words _uint64))
(define c-docs (list->cblock docs _uint64))
(define zsc (list->cblock topics _uint64))

(define (for-doc doc)
  (printf "calling prog\n")
  (define init-time (get-time))
  (define output-c (prog topic-prior word-prior zsc c-words c-docs doc))
  (define el-time (elasp-time init-time))

  (printf "output: [")
  (for ([i (in-range num-topics)])
    (printf "~a, " (get-index-prob-array output-c i)))
  (printf "  ]\n")
  (printf "time taken: ~a\n" el-time)
  output-c)


(for-doc 9900)
;; output for 9900
;; (-458.1908976216084 -461.9810000474454 -478.57190842283467 -446.5395681936905 -453.6747904210667 -463.4369314698437 -450.8619961395306 -453.16976113041795 -445.12265609048325 -374.5512758505581 -458.091812993318 -447.1539476558063 -454.3902748158387 -450.796948060257 -448.8193584047232 -450.2865595520607 -450.52765701408424 -460.69817293449074 -453.0247297820897 -457.1166709136238)
