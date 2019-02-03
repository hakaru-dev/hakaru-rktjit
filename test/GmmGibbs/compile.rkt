#lang racket

(require sham
         sham/jit-utils
         hakrit/jit
         hakrit/utils)
(require ffi/unsafe)
(define fname "input.hkr")
(define input-dir "./nocat")

(define input '([-3.8907259534842202
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
(define classes 9)
(define points 10)

(define stdev (real->prob 14.0))
(define zs (make-sized-hakrit-array (cdr input) 'nat))
(define as (make-sized-hakrit-array (list 0.0 0.0 0.0
                                          0.0 0.0 0.0
                                          0.0 0.0 0.0) 'real))
(define t (make-sized-hakrit-array (car input) 'real))
(define doc 0)

(define (run input-dir fname)
  (define module-env (compile-file (build-path input-dir fname) '()))
  (define prog (get-prog module-env))

  (define result (prog stdev as zs t doc))
  (sized-hakrit-array->racket-list result 'prob))

(module+ test
  (require rackunit)
  (define nocat-result (run "./nocat" "input.hkr"))
  (define nocat-expected
    '(268.0596789212606 272.15138815387155 272.15138815387155 272.15138815387155 274.6138467723381 272.15138815387155 193.6259482527394 256.24364009217334 272.15138815387155))
  (map (λ (v1 v2) (check-= v1 v2 0.00001 "GmmGibbs.nocat")) nocat-result nocat-expected)

  (define 1half-result (run "./1half" "input.hkr"))
  (define 1half-expected
    '(-10.818090761678354 -14.355996628824293 -14.355996628824293 -14.355996628824293 -10.013711119074703 -14.355996628824293 -11.36655052546099 -11.36655052546099 -14.355996628824293))
  (map (λ (v1 v2) (check-= v1 v2 0.00001 "GmmGibbs.1half")) 1half-result 1half-expected)

  (define 2half-result (run "./2half" "input.hkr"))
  (define 2half-expected '(278.877769682939 286.50738478269585 286.50738478269585 286.50738478269585 284.6275578914128 286.50738478269585 204.99249877820037 267.6101906176343 286.50738478269585))
  (map (λ (v1 v2) (check-= v1 v2 0.00001 "GmmGibbs.2half")) 2half-result 2half-expected))
