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

(define module-env (debug-file (build-path input-dir fname) '()))
(begin
  (define (gf id) (get-function module-env id))
  (define array-make (gf 'array-make))
  (define array-clear (gf 'array-clear))
  (define array-free (gf 'array-free))
  (define array-size (gf 'array-get-size))
  (define array-ref (gf 'array-ref))
  (define array-set! (gf 'array-set!)))

(define prog (get-prog module-env))

(define result (prog stdev as zs t doc))
(pretty-print (sized-hakrit-array->racket-list result 'prob))


(define nocat
  '(268.0596789212606 272.15138815387155 272.15138815387155 272.15138815387155 274.6138467723381 272.15138815387155 193.6259482527394 256.24364009217334 272.15138815387155))
(define 1half
  '(-10.818090761678354 -14.355996628824293 -14.355996628824293 -14.355996628824293 -10.013711119074703 -14.355996628824293 -11.36655052546099 -11.36655052546099 -14.355996628824293))
(define 2half '(278.877769682939 286.50738478269585 286.50738478269585 286.50738478269585 284.6275578914128 286.50738478269585 204.99249877820037 267.6101906176343 286.50738478269585))



;; bucket manual
;; (define bk (make-sized-hakrit-array (build-list 10 (const 0)) 'nat))
;; (for ([i (range 10)]) (array-set! bk (array-ref zs i) (+ (array-ref bk (array-ref zs i)) 1)))


;; (define ta (make-sized-hakrit-array (build-list 10 (const 5.0)) 'real))
;; (sized-hakrit-array->racket-list ta 'real)
;; ;; '(5.0 5.0 5.0 5.0 5.0 5.0 5.0 5.0 5.0 5.0)
;; (array-clear ta)
;; (sized-hakrit-array->racket-list ta 'real)
;; ;; '(0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 5.0)


;; (define tb (make-sized-hakrit-array (build-list 10 (const 5)) 'nat))
;; (sized-hakrit-array->racket-list tb 'nat)
;; ;; '(5 5 5 5 5 5 5 5 5 5)
;; (array-clear tb)
;; (sized-hakrit-array->racket-list tb 'nat)
;; ;; '(0 0 0 0 0 0 0 0 0 0)
