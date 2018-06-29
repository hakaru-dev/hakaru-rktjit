#lang racket
(require ffi/unsafe
         rackunit
         racket/runtime-path)

(require hakrit
         hakrit/utils)
(define input-dir "../../../input/")
(define news-dir "news_t/")

(define wordsfile  (build-path input-dir news-dir "words"))
(define docsfile   (build-path input-dir news-dir "docs"))
(define topicsfile   (build-path input-dir news-dir "topics"))

(define rk-words (map string->number (file->lines wordsfile)))
(define rk-docs (map string->number (file->lines docsfile)))
(define rk-topics (map string->number (file->lines topicsfile)))

(define words-size (length rk-words))
(define docs-size (length rk-docs))
(define topics-size (length rk-topics))
(define num-docs (add1 (last rk-docs)))
(define num-words (add1 (argmax identity rk-words)))
(define num-topics (add1 (argmax identity rk-topics)))

  (printf "num-docs: ~a, num-words: ~a, num-topics: ~a\n" num-docs num-words num-topics)
  (printf "words-size: ~a, docs-size: ~a, topics-size: ~a\n" words-size docs-size topics-size)
(define full-info
  `(((array-info . ((size . ,num-topics))))
    ((array-info . ((size . ,num-words))))
    ()
    ;; ((nat-info . ((value . ,num-docs))))
    ((array-info . ((size . ,words-size)
                    (value . ,rk-words))))
    ((array-info . ((size . ,words-size)
                    (value . ,rk-docs))))
    ((array-info . ((size . ,words-size))))
    ((nat-info . ((value-range . (0 . ,(- num-words 1))))))))

(define prog (compile-hakaru "./partial2.hkr" full-info))

(define (run-test)
  (define topics-prior (list->cblock (build-list num-topics (const 0.0)) _double))
  (printf "made topics-prior\n")
  (define words-prior (list->cblock (build-list num-words (const 0.0)) _double))
  (printf "made words-prior\n")

  (define words (list->cblock rk-words _uint64))
  (printf "made words\n")
  (define docs (list->cblock rk-docs _uint64))
  (printf "made docs\n")

  (define z (list->cblock (build-list (length rk-words) (const 1)) _uint64))
  (define word-update 1)
  (define result (prog topics-prior words-prior num-docs words docs z word-update))
  result)

(define result (run-test))

;; (module+ test
;;   (define our-out (run-full-test 42))
;;   (printf "output: ~a\n" our-out))

;; (module+ main
;;   (define our-out (run-full-test 42))
;;   (printf "output: ~a\n" our-out))
