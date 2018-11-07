#lang racket

(require "../../../sham/private/ast-utils.rkt"
         "../../../sham/private/info.rkt"
         "../../../sham/private/parameters.rkt")
(require "template-format.rkt"
         "type-defines.rkt"
         "basic-defines.rkt"
         "utils.rkt")

(provide pair-defs
         pair-rator?
         get-pair-rator)

(define (pair-rator? sym)
  (member sym '(car cdr cons set-car! set-cdr! free-pair)))

(define (get-pair-rator sym pair-defs)
  (match-define (list cons-pair free-pair car-pair cdr-pair set-car-pair set-cdr-pair) pair-defs)
  (match sym
    ['car car-pair]
    ['cdr cdr-pair]
    ['set-car! set-car-pair]
    ['set-cdr! set-cdr-pair]
    ['cons cons-pair]
    ['free-pair free-pair]))

(define pair-struct (tstruct '(a b) (list i64 i64)))
(define pair-type  (tptr pair-struct))
(define data-type i64)

(define-sham-function
  (pair-cons (a : data-type) (d : data-type)) : pair-type
  (slet^ ([pp  (malloc^ (etype pair-struct)) : pair-type])
         (store! a (get-struct-field pp 0))
         (store! d (get-struct-field pp 1))
         (ret pp)))
(define-sham-function
 (pair-free (p : pair-type)) : tvoid
 (free^ p)
 ret-void)
(define-sham-function
 (pair-car (p : pair-type)) : data-type
  (ret (load (get-struct-field p 0))))
(define-sham-function
  (pair-cdr (p : pair-type)) : data-type
  (ret (load (get-struct-field p 1))))
 (define-sham-function
  (pair-set-car!
   (p : pair-type) (a : data-type)) : tvoid
  (store! a (get-struct-field p 0))
  ret-void)
(define-sham-function
 (pair-set-cdr!
  (p : pair-type) (d : data-type)) : tvoid
 (store! d (get-struct-field p 1))
 ret-void)

(module+ test
  (require rackunit
           "../../../sham/private/jit-utils.rkt")
  (parameterize ([compile-options `(dump verify mc-jit)])
    (compile-sham-module!
     (current-sham-module)
     #:opt-level 0))
  (define pc (sham-app pair-cons 3 4))
  (check-equal? (sham-app pair-car pc) 3)
  (check-equal? (sham-app pair-cdr pc) 4)
  (sham-app pair-set-car! pc 42)
  (check-equal? (sham-app pair-car pc) 42)
  (sham-app pair-set-cdr! pc 21)
  (check-equal? (sham-app pair-cdr pc) 21)
  (sham-app pair-free pc))




(define (pair-defs tast)
  (match-define `(pair ,ta ,td) tast)
  (define st (sham-type tast))
  (define stp (tptr st ))
  (define sta (sham-type ta))
  (define std (sham-type td))
  (common-function-info (prelude-function-info))

  (define cons-pair
    (sham-function
     (,(get-function-id cons-format tast) ;;cons-pair
      (a : sta) (d : std)) : stp
     (slet^ ([pp  (malloc^ (etype st)) : stp])
            (store! a (get-struct-field pp 0))
            (store! d (get-struct-field pp 1))
            (ret pp))))

  (define free-pair
    (sham-function
     (,(get-function-id free-pair-format tast)
      (p : stp)) : tvoid
     (free^ p)
     ret-void))
  (define car-pair
    (sham-function
     (,(get-function-id car-format tast)
      (p : stp)) : sta
     (ret (load (get-struct-field p 0)))))
  (define cdr-pair
    (sham-function
     (,(get-function-id cdr-format tast)
      (p : stp)) : std
     (ret (load (get-struct-field p 1)))))
  (define set-car-pair
    (sham-function
     (,(get-function-id set-car-format tast)
      (p : stp) (a : sta)) : tvoid
     (store! a (get-struct-field p 0))
     ret-void))
  (define set-cdr-pair
    (sham-function
     (,(get-function-id set-cdr-format tast)
      (p : stp) (d : std)) : tvoid
     (store! d (get-struct-field p 1))
     ret-void))
  (list cons-pair free-pair car-pair cdr-pair set-car-pair set-cdr-pair))

(module+ test
  (require rackunit
           "../../../sham/private/jit-utils.rkt")

  (define simple-test-module (create-empty-sham-module "pair-test-mdoule"))
  (current-sham-module simple-test-module)
  (define pdfs (pair-defs '(pair nat nat)))
  (map (curry add-to-sham-module! (current-sham-module)) pdfs)
  (parameterize ([compile-options `(pretty dump verify mc-jit)])
    (compile-sham-module!
     (current-sham-module)
     #:opt-level 0))

  (match-define (list consnn freenn carnn cdrnn setcarnn setcdrnn) pdfs)
  (define tp (sham-app consnn 3 4))
  (check-equal? (sham-app carnn tp) 3)
  (check-equal? (sham-app cdrnn tp) 4)
  (sham-app setcarnn tp 42)
  (sham-app setcdrnn tp 21)
  (check-equal? (sham-app carnn tp) 42)
  (check-equal? (sham-app cdrnn tp) 21)
  (sham-app freenn tp))
