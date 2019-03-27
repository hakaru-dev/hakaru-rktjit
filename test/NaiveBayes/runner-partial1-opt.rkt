#lang racket
(require sham
         sham/jit-utils
         hakrit/jit
         hakrit/utils)

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

(define topic-prior (make-fixed-hakrit-array (build-list num-topics (const 0.0)) 'real))
(define word-prior (make-fixed-hakrit-array (build-list num-words (const 0.0)) 'real))

(define c-words (make-fixed-hakrit-array words 'nat))
(define c-docs (make-fixed-hakrit-array docs 'nat))
(define zsc (make-fixed-hakrit-array topics 'nat))

(define full-info
  `(
    (topic_prior . ((array-info . ((size . ,num-topics)))))
    (word_prior . ((array-info . ((size . ,num-words)))))
    (z . ((array-info . ((size . ,num-docs)))))
    (w . ((array-info . ((size . ,words-size)))))
    (doc . ((array-info . ((size . ,words-size)))))
    ))

(define module-env (compile-file "./partial2.hkr" full-info))
(printf "module compiled\n")
(define prog (get-prog module-env))

(module+ test
  (require rackunit)
  (printf "running prog\n")
  (define result (prog topic-prior word-prior zsc c-words c-docs 9900))
  (printf "got result\n")
  (define nocat-result (fixed-hakrit-array->racket-list result 'prob 20))
  (pretty-display nocat-result)
  (define nocat-expected '(81.13020809116642
                           75.49329450576455
                           77.37990328377306
                           79.42381708972944
                           69.80226757429736
                           77.3238933802139
                           69.16354905595196
                           74.78024716851174
                           81.21301607112039
                           154.82742570957714
                           78.4744091006872
                           94.79851070676456
                           72.5266684002621
                           88.08264202709779
                           91.11054327393825
                           92.4930633276475
                           91.62033709479157
                           95.02414174329836
                           94.86645039152441
                           83.33978177944539))
  (map (λ (v1 v2) (check-= v1 v2 0.00001 "NaiveBayes.nocat")) nocat-result nocat-expected)
  )
