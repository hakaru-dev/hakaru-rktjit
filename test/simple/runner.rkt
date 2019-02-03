#lang racket

(require sham
         sham/jit-utils
         hakrit/jit
         hakrit/utils)

(define fname "array-product.hkr")
(define input-dir "./")
(define input-array-real (list 1.0 2.0 3.0 4.0 5.0))
(define input-array (map real->prob input-array-real))
(define sized-input-array (make-sized-hakrit-array input-array 'real))
(define fixed-input-array (make-fixed-hakrit-array input-array 'real))

(define (compile-and-run fname inputs (info '()))
  (printf "compiling: ~a with info: ~a\n" fname info)
  (define module-env (debug-file (build-path input-dir fname) info))
  (define prog (get-prog module-env))
  (apply prog inputs))

(module+ test
  (require rackunit)
  (define ags "array-get-size.hkr")
  (define arr "array.hkr")
  (define arp "array-product.hkr")
  (define ars  "array-summate.hkr")

  (begin
    (printf "normals\n")
    (begin
      (printf "normal array-get-size\n")
      (define nresult-ags (compile-and-run ags (list sized-input-array)))
      (check-equal? nresult-ags (length input-array)))

    (begin
      (printf "normal array\n")
      (define nresult-arr
        (sized-hakrit-array->racket-list (compile-and-run arr (list sized-input-array)) 'prob))
      (map (λ (v1 v2) (check-= v1 v2 0.00001)) nresult-arr input-array)
      (map (λ (v1 v2) (check-= (prob->real v1) (prob->real v2) 0.00001)) nresult-arr input-array))

    (begin
      (printf "normal array-product\n")
      (define nresult-arp (compile-and-run arp (list sized-input-array)))
      (check-equal? nresult-arp (foldr + 0 input-array)))

    (begin
      (printf "normal array-summate\n")
      (define nresult-ars (compile-and-run ars (list sized-input-array)))
      (check-equal? (prob->real nresult-ars) (foldr + 0 input-array-real))))

  (begin
    (printf "optimized\n")
    (define info `((z . ((array-info . ((size . ,(length input-array))))))))

    (begin
      (printf "optimized array-get-size\n")
      (define result-ags (compile-and-run ags (list sized-input-array) info))
      (check-equal? result-ags (length input-array)))

    (begin
      (printf "optimized array\n")
      (define raw-result-arr  (compile-and-run arr (list fixed-input-array) info))
      (define result-arr (fixed-hakrit-array->racket-list raw-result-arr 'prob (length input-array)))
      (map (λ (v1 v2) (check-= v1 v2 0.00001)) result-arr input-array)
      (map (λ (v1 v2) (check-= (prob->real v1) (prob->real v2) 0.00001)) result-arr input-array))

    (begin
      (printf "optimized array-product\n")
      (define result-arp (compile-and-run arp (list fixed-input-array) info))
      (check-equal? result-arp (foldr + 0 input-array)))

    (begin
      (printf "optimized array-summate\n")
      (define result-ars (compile-and-run ars (list fixed-input-array) info))
      (check-equal? (prob->real result-ars) (foldr + 0 input-array-real)))))
