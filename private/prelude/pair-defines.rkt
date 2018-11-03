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

(define (pair-defs tast)
  (match-define `(pair ,ta ,td) tast)
  (define stp (tptr (sham-type tast)))
  (define sta (sham-type ta))
  (define std (sham-type td))
  (common-function-info (prelude-function-info))

  (define cons-pair
    (sham-function
     (,(get-function-id cons-format tast) ;;cons-pair
      (a : sta) (d : std)) : stp
     (slet^ ([pp  (malloc^ (etype stp)) : stp])
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
