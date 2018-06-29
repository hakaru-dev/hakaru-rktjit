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
      ((array-info . ((size . ,points))))
      ((array-info . ((size . ,points)))
       (attrs . (constant))
       (value . ,(car input)))
      ()))

  ;; (define prog (compile-hakaru
  ;;               (build-path current-dir "partial1.hkr")
  ;;               ;; `(s  . (value . 0.0))
  ;;               ;;     `(as . (value . (0.0 0.0 0.0)))
  ;;               `(z  . (size . ,classes))
  ;;               `(t  . (value . ,(car input)))))
  (define module-env (compile-file (build-path current-dir "partial1.hkr") full-info))
  (define prog (jit-get-function 'prog module-env))
  ;; (define prog (jit-get-function 'prog partial1-env))

  ;; (jit-dump-module partial1-env )
  ;; (jit-dump-function partial1-env 'prog)
  ;; (define module-env partial1-env)
  ;; (define real2prob (jit-get-function (string->symbol "real2prob") module-env))
  ;; (define prob2real (jit-get-function (string->symbol "prob2real") module-env))

  (define stdev (real->prob 14.0))
  (define zs (make-fixed-hakrit-array (cdr input) 'nat))
  (define as (make-fixed-hakrit-array (list 0.0 0.0 0.0) 'real))
  (define t (make-fixed-hakrit-array (car input) 'real))
  (define doc 0)

  (define output-c (prog stdev as zs doc))
  ;; (define get-index-3-prob (jit-get-function (string->symbol (format "get-index$array<~a.prob>" classes)) partial1-env))
  (define output-list (map identity (cblock->list output-c _double classes)))
  output-list)

(module+ test
  (require rackunit)
  (define our-out (run-with-info input-9-10))
  ;; (define _ (map (curryr check-= 0.0000000000000001) our-out
  ;;                '[2.611195425608999e116
  ;;                  1.5625938746100886e118
  ;;                  1.5625938746100886e118
  ;;                  1.5625938746100886e118
  ;;                  1.8334889944812062e119
  ;;                  1.5625938746100886e118
  ;;                  1.2321990778726571e84
  ;;                  1.9284079571442863e111
  ;;                  1.5625938746100886e118]))
  our-out)

;; [268.0596789212606,272.15138815387155,272.15138815387155,272.15138815387155,274.6138467723381,272.15138815387155,193.6259482527394,256.24364009217334,272.15138815387155]
