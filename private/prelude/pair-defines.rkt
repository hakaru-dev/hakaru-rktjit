#lang racket

(require sham
         (submod sham/ast utils))
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
  (match-define (pair ,ta ,td) tast)
  (define stp (sham-type tast))
  (define sta (sham-type ta))
  (define std (sham-type td))

  (define cons-pair
    (sham-function
     #:info (prelude-function-info)
     (,(get-function-id cons-pair-format tast) ;;cons-pair
      (a : sta) (d : std)) : stp
     (slet^ ([pp  (malloc (etype pt)) : stp])
            (store! a (struct-field pp 0))
            (store! d (struct-field pp 1))
            (ret pp))))

  (define free-pair
    (sham-function
     #:info (prelude-function-info)
     (,(get-function-id free-pair-format tast)
      (p : stp)) : tvoid
     (free p)
     ret-void))
  (define car-pair
    (sham-function
     #:info (prelude-function-info)
     (,(get-function-id car-pair-format tast)
      (p : stp)) : sta
     (ret (load (struct-field p 0)))))
  (define cdr-pair
    (sham-function
     #:info (prelude-function-info)
     (,(get-function-id car-pair-format tast)
      (p : stp)) : std
     (ret (load (struct-field p 1)))))
  (define set-car-pair
    (sham-function
     #:info (prelude-function-info)
     (,(get-function-id set-car-pair-format tast)
      (p : stp) (a : sta)) : tvoid
     (store! a (struct-field p 0))
     ret-void))
  (define set-cdr-pair
    (sham-function
     #:info (prelude-function-info)
     (,(get-function-id set-car-pair-format tast)
      (p : stp) (d : std)) : tvoid
     (store! d (struct-field p 1))
     ret-void))
  (list cons-pair free-pair car-pair cdr-pair set-car-pair set-cdr-pair))

;; (module+ test
;;   (require rackunit
;;            sham/jit
;;            ffi/unsafe)

;;   (define defs
;;     (apply append
;;            (map pair-defs
;;                 `((pair nat nat)
;;                   (pair (pair nat nat) nat)))))
;; ;  (pretty-print (map sham-def->sexp defs))
;;   (define mod
;;     (sham:module (basic-mod-info) defs))
;;   (define cmod (compile-module mod))
;;   (optimize-module cmod)

;;   (jit-dump-module cmod)
;;   (jit-verify-module cmod)
;;   (initialize-jit! cmod)

;;   (define (get-t t) (jit-get-racket-type t cmod))
;;   (define (get-f f) (jit-get-function f cmod))
;;   (define ((gf frmt) tsym)
;;     (get-f (get-fun-symbol frmt (get-type-string tsym))))

;;   (define t-nat (get-t 'nat))
;;   (define make-f (gf cons-pair-fun-format))
;;   (define car-f (gf pair-car-fun-format))
;;   (define cdr-f (gf pair-cdr-fun-format))

;;   (define pnn `(pair nat nat))
;;   (define cons-pair-nn (make-f pnn))
;;   (define car-pair-nn (car-f pnn))
;;   (define cdr-pair-nn (cdr-f pnn))
;;   (define tp (cons-pair-nn 24 42))
;;   (check-eq? (car-pair-nn tp) 24)
;;   (check-eq? (cdr-pair-nn tp) 42)

;;   (define ppn `(pair (pair nat nat) nat))
;;   (define cons-pair-ppn (make-f ppn))
;;   (define car-pair-ppn (car-f ppn))
;;   (define cdr-pair-ppn (cdr-f ppn))
;;   (define tpp (cons-pair-ppn tp 84))
;;   (check-eq? (car-pair-nn (car-pair-ppn tpp)) 24)
;;   (check-eq? (cdr-pair-ppn tpp) 84))
