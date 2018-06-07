#lang racket
(require ffi/unsafe
         racket/runtime-path)

(require
 hakrit/jit
 hakrit/utils
 sham)

(define input-3-10 '([3.8728103253204136
                      1.1452918218810444
                      -0.37443733246614497
                      2.2524280674567634
                      0.1088871787126991
                      2.2484645323958334
                      0.19013878436498044
                      1.4032911741452248
                      2.1930977191694936
                      1.7312282946567383]
                     .
                     [2 2 2 2 1 2 2 2 2 2]))

(define input-9-10 '([-3.8907259534842202
                      -6.618193122202014
                      -4.941794905899988
                      -7.119531519149578
                      -5.9345748166212
                      4.825733087265147
                      -8.331891801596736
                      -5.597647903988242
                      -9.082321592213654
                      14.219895056304555]
                     .
                     [4 4 4 4 4 7 0 4 0 6]))
(define empty-info-3-10 '(() () () () () ()))
(define classes 3)
(define points 10)
(define-runtime-path current-dir "./")
(define pair-array-regex "^\\(\\[(.*)\\],\\[(.*)\\]\\)$")

(define (run-with-info input)
  (define points (length (car input)))
  (define classes 9)
  (define full-info
    `(()
      ((array-info . ((size . ,classes))))
      ((array-info
        . ((size . ,points)
           (elem-info
            . ((nat-info
                . ((value-range . (0 . ,(- classes 1))))))))))
      ((array-info . ((size . ,points)))
       (attrs . (constant))
       (value . ,(car input)))
      ((nat-info . ((value-range . (0 . ,(- points 1))))))))

  (define module-env (compile-file (build-path current-dir "partial1.hkr") full-info))
  (define prog (jit-get-function 'prog module-env))

  (jit-dump-module module-env )
  (jit-dump-function module-env 'prog)

  (define real2prob (jit-get-function (string->symbol "real2prob") module-env))
  (define prob2real (jit-get-function (string->symbol "prob2real") module-env))

  (define stdev (real2prob 14.0))
  (define zs (list->cblock (cdr input) _uint64))
  (define as (list->cblock (list 0.0 0.0 0.0) _double))
  (define doc 0)

  (printf "calling prog:\n")
  (define output-c (prog stdev as zs doc))
  (define output-list  (cblock->list output-c _double 3))
  output-list)

(module+ test
  (require rackunit)
  (define our-out (run-with-info input-9-10)) our-out)

#;[268.0596789212606
   272.15138815387155
   272.15138815387155
   272.15138815387155
   274.6138467723381
   272.15138815387155
   193.6259482527394
   256.24364009217334
   272.15138815387155]
