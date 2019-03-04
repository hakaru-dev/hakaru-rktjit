#lang racket

(require sham/private/ast-utils
         sham/private/ast-info
         sham/private/jit-utils
         sham/private/parameters)
(require "template-format.rkt"
         "type-defines.rkt"
         "basic-defines.rkt"
         "utils.rkt")

(provide pair-defs
         pair-rator?
         get-pair-rator)

(define (pair-rator? sym)
  (member sym '(car cdr cons set-car! set-cdr! free-pair)))



(define pair-struct (tstruct '(a b) (list i64 i64)))
(define pair-type  (tptr pair-struct))
(define data-type i64)

(define (get-pair-rator rator tresult trands)
  (match rator
    ['car (λ (p) (load (get-struct-field p 0)))]
    ['cdr (λ (p) (load (get-struct-field p 1)))]
    ['set-car! (λ (p a) (store! a (get-struct-field p 0)))]
    ['set-cdr! (λ (p a) (store! a (get-struct-field p 0)))]
    ['cons (λ (a b) (let^ ([pp (malloc^ (etype (tstruct '(a b) (list (get-sham-type (first trands))
                                                                     (get-sham-type (second trands))))))
                               : (get-sham-type tresult)]) ; TODO verify
                          (store! a (get-struct-field pp 0))
                          (store! b (get-struct-field pp 1))
                          pp))]
    ['free-pair (λ (p) (free^ p))]))


(define-sham-function
  (pair-cons (a : data-type) (d : data-type) : pair-type)
  (slet^ ([pp  (malloc^ (etype pair-struct)) : pair-type])
         (store! a (get-struct-field pp 0))
         (store! d (get-struct-field pp 1))
         (ret pp)))
(define-sham-function
  (pair-free (p : pair-type) : tvoid)
  (free^ p)
  (return-void))
(define-sham-function
  (pair-car (p : pair-type) : data-type)
  (ret (load (get-struct-field p 0))))
(define-sham-function
  (pair-cdr (p : pair-type) : data-type)
  (ret (load (get-struct-field p 1))))
 (define-sham-function
   (pair-set-car!
    (p : pair-type) (a : data-type) : tvoid)
  (store! a (get-struct-field p 0))
  (return-void))
(define-sham-function
  (pair-set-cdr!
   (p : pair-type) (d : data-type) : tvoid)
 (store! d (get-struct-field p 1))
 (return-void))


(define (pair-defs tast)
  (match-define `(pair ,ta ,td) tast)
  (define st (get-sham-type tast))
  (define stp st)
  (define sta (get-sham-type ta))
  (define std (get-sham-type td))
  (common-function-info (prelude-function-info))

  (define cons-pair
    (sham-function
     (,(get-function-id cons-format tast) ;;cons-pair
      (a : sta) (d : std) : stp)
     (return ((get-pair-rator 'cons tast (list ta td)) a d))))

  (define free-pair
    (sham-function
     (,(get-function-id free-pair-format tast)
      (p : stp) : tvoid)
     (free^ p)
     (ret-void)))
  (define car-pair
    (sham-function
     (,(get-function-id car-format tast)
      (p : stp) : sta)
     (ret (load (get-struct-field p 0)))))
  (define cdr-pair
    (sham-function
     (,(get-function-id cdr-format tast)
      (p : stp) : std)
     (ret (load (get-struct-field p 1)))))
  (define set-car-pair
    (sham-function
     (,(get-function-id set-car-format tast)
      (p : stp) (a : sta) : tvoid)
     (store! a (get-struct-field p 0))
     (ret-void)))
  (define set-cdr-pair
    (sham-function
     (,(get-function-id set-cdr-format tast)
      (p : stp) (d : std) : tvoid)
     (store! d (get-struct-field p 1))
     (ret-void)))
  (list cons-pair free-pair car-pair cdr-pair set-car-pair set-cdr-pair))

(module+ test
  (require rackunit)
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
  (sham-app freenn tp)

  (require ffi/unsafe)
  (define (cons-hakrit-array-pair a b)
    (define p (list->cblock  '(0 0) _uint64))
    (ptr-set! p _pointer 0 a)
    (ptr-set! p _pointer 1 b)
    p)
  (define (car-hakrit-array-pair pr)
    (ptr-ref pr _pointer 0))
  (define (cdr-hakrit-array-pair pr)
    (ptr-ref pr _pointer 1))
  (define a (list->cblock '(1 2 3) _uint64))
  (define b (list->cblock '(4 5 6) _uint64))
  (define hp (cons-hakrit-array-pair a b)))
