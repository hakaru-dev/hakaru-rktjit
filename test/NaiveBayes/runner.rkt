#lang racket
(require sham
         sham/jit-utils
         hakrit/jit
         hakrit/utils
         rackunit
         racket/runtime-path)

(define module-env (compile-file "./NaiveBayesGibbs.hkr" '()))
(define prog (get-prog module-env))

(define news-dir "./news/")
(define words (map string->number (file->lines (build-path news-dir "words"))))
(define docs (map string->number (file->lines (build-path news-dir "docs"))))
(define topics (map string->number (file->lines (build-path news-dir "topics"))))

(define words-size (length words))
(define docs-size (length docs))
(define topics-size (length topics))

(define num-docs (add1 (last docs)))
(define num-words (add1 (argmax identity words)))
(define num-topics (add1 (argmax identity topics)))

(define topic-prior (make-sized-hakrit-array (build-list num-topics (const 0.0)) 'real))
(define word-prior (make-sized-hakrit-array (build-list num-words (const 0.0)) 'real))

(define c-words (make-sized-hakrit-array words 'nat))
(define c-docs (make-sized-hakrit-array docs 'nat))
(define zsc (make-sized-hakrit-array topics 'nat))


(module+ test
  (require rackunit)
  (define result (prog topic-prior word-prior zsc c-words c-docs 99))
  (for/list ([i (in-range 10)])
    (prog topic-prior word-prior zsc c-words c-docs 99))
  (printf "result: ~a\n" result)
  )
