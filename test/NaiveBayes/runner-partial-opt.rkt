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

(define topic-prior (make-sized-hakrit-array (build-list num-topics (const 0.0)) 'real))
(define word-prior (make-sized-hakrit-array (build-list num-words (const 0.0)) 'real))

(define c-words (make-sized-hakrit-array words 'nat))
(define c-docs (make-sized-hakrit-array docs 'nat))
(define zsc (make-sized-hakrit-array topics 'nat))

(define full-info
  `((topic_prior . ((array-info . ((size . ,num-topics)))))
    (word_prior . ((array-info . ((size . ,num-words)))))
    (z . ((array-info . ((size . ,num-docs)
                         ;; (elem-info . ((nat-info
                         ;;            . ((value-range
                         ;;                . (0 . ,(- num-topics 1)))))))
                         ))))
    (w . ((array-info . ((size . ,words-size)
                         ;; (elem-info . ((nat-info . ((value-range . (0 . ,(- num-words 1)))))))
                         ;; (value . ,words)
                     ))))
    (doc . ((array-info . ((size . ,words-size)
                     ;; (elem-info . ((nat-info
                     ;;                . ((value-range
                     ;;                    . (0 . ,(- num-docs 1)))))))
                     ;; (value . ,docs)
                     ))))
    ;; (docUpdate . ((nat-info . ((value-range . (0 . ,(- num-docs 1)))))))
    ))

(define module-env (compile-file "./partial.hkr" full-info))
(printf "module compiled\n")
(define prog (get-prog module-env))

(module+ test
  (require rackunit)
  (define result (prog topic-prior word-prior zsc c-words c-docs 9900))
  (define nocat-result (sized-hakrit-array->racket-list result 'prob))
  (define nocat-expected '(-458.1908976216084 -461.9810000474454 -478.57190842283467 -446.5395681936905 -453.6747904210667 -463.4369314698437 -450.8619961395306 -453.16976113041795 -445.12265609048325 -374.5512758505581 -458.091812993318 -447.1539476558063 -454.3902748158387 -450.796948060257 -448.8193584047232 -450.2865595520607 -450.52765701408424 -460.69817293449074 -453.0247297820897 -457.1166709136238))
  (map (λ (v1 v2) (check-= v1 v2 0.00001 "NaiveBayes.nocat")) nocat-result nocat-expected))
