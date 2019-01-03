#lang racket
(require ffi/unsafe)
(require sham/jit)
(require "ast.rkt")
;; (require "ast.rkt"
;;          "prelude/type-defines.rkt"
;;          "prelude/template-format.rkt"
;; )

;; (require (for-syntax racket/syntax))
(provide (all-defined-out))

(define (prob->real x) (exp x))
(define (real->prob x) (log x))
(define (nat->prob x) (real->prob (exact->inexact x)))

;; (define (logsumexp2 a b)
;;   (if (> a b)
;;       (+ a (log (exp (- b a))))
;;       (+ b (log (exp (- a b))))))

;; (define (one-of-type t)
;;   (if (equal? t 'prob)
;;       (real->prob 1.0)
;;       1.0))
;; (define (zero-of-type t)
;;   (if (equal? t 'prob)
;;       (real->prob 0.0)
;;       0.0))

;; (define logspace-add
;;   (λ args
;;     (real->prob (apply + (map prob->real args)))))

;; (define (replicate-vector n i)
;;   (build-vector n (const i)))

;; (define (read-vector-from-csv fname)
;;   (call-with-input-file fname
;;     (lambda (in)
;;       (for/vector [(s (in-lines in))]
;;         (string->number s)))))

;; (define (get-cmd-argument i)
;;   (vector-ref (current-command-line-arguments) i))

;; ;; utils for running jit
;; (define (compile-hakaru file-name info)
;;   (define module-env (compile-file file-name info))
;;   ;; (jit-dump-module module-env)
;;   (define init-rng (jit-get-function 'init-rng module-env))
;;   (init-rng)
;;   (define prog (jit-get-function 'prog module-env))
;;   prog)

;; (define (get-array-function-sym f t)
;;   (define ts (get-type-string t))
;;   (string->symbol
;;    (match f
;;      ['get-index (format get-index-fun-format ts)]
;;      ['get-size (format get-array-size-fun-format ts)]
;;      ['get-data (format get-array-data-fun-format ts)]
;;      ['set-index! (format set-index-fun-format ts)]
;;      ['make (format make-array-fun-format ts)]
;;      ['new (format new-size-array-fun-format ts)])))

;; (define (get-pair-function-sym f t)
;;   (define ts (get-type-string t))
;;   (string->symbol
;;    (match f
;;      ['make (format make-pair-fun-format ts)]
;;      ['car (format pair-car-fun-format ts)]
;;      ['cdr (format pair-cdr-fun-format ts)]
;;      ['set-car! (format pair-set-car-fun-format ts)]
;;      ['set-cdr! (format pair-set-cdr-fun-format ts)])))

;; (define (get-function-sym module-env sym t)
;;   (jit-get-function (if (equal? (car t) 'pair)
;;                            (get-pair-function-sym sym t)
;;                            (get-array-function-sym sym t))
;;                        module-env))

;; (define (get-racket-type t)
;;   (match t
;;     ['real _double]
;;     ['prob _double]
;;     ['nat _uint64]
;;     ['unit _uint64]
;;     [else _pointer]))

;; (define (rkt->jit module-env type val)
;;   (match type
;;     [`(array ,t)
;;      (define size (length val))
;;      (define arr ((get-function-sym module-env 'new type) size))
;;      (for ([j (in-range size)]
;;            [v val])
;;        ((get-function-sym module-env 'set-index! type)
;;         arr j (rkt->jit module-env t v)))
;;      arr]
;;     [`(pair ,ta ,tb)
;;      (define tav (rkt->jit module-env ta (car val)))
;;      (define tbv (rkt->jit module-env tb (cdr val)))
;;      ((get-function-sym module-env 'make type) tav tbv)]
;;     ['prob ((jit-get-function 'real2prob module-env) (exact->inexact val))]
;;     ['real (exact->inexact val)]
;;     [else val]))

;; (define (jit->rkt module-env type val)
;;   (match type
;;     [`(array ,t)
;;      (define size ((get-function module-env 'get-size type) val))
;;      (define data ((get-function module-env 'get-data type) val))
;;      (printf "array-size: ~a\n" size)
;;      (for/list ([j (in-range size)])
;;        (jit->rkt module-env t ((get-function module-env 'get-index type) val j)))]
;;     [`(pair ,ta ,tb)
;;      (define tav
;;        (jit->rkt module-env ta ((get-function 'module-env 'car type) val)))
;;      (define tbv
;;        (jit->rkt module-env tb ((get-function 'module-env 'cdr type) val)))
;;      (cons tav tbv)]
;;     [`(pointer ,t)
;;      (jit->rkt module-env t val)]
;;     ['prob ((jit-get-function 'prob2real module-env) val)]
;;     [else val]))

(define (rkt-type t)
  (match t
    ['nat _uint64]
    ['prob _double]
    ['real _double]))

(define (make-fixed-hakrit-array arr type) (list->cblock arr (rkt-type type)))
(define (make-sized-hakrit-array arr type)
  (define ret (list->cblock (cons (car arr) arr) (rkt-type type)))
  (ptr-set! ret _uint64 0 (length arr))
  ret)

(define (fixed-hakrit-array-ref arr type index) (ptr-ref arr (rkt-type type) index))
(define (fixed-hakrit-array-set! arr type index value) (ptr-set! arr (rkt-type type) index value))

(define (sized-hakrit-array-ref arr type index) (ptr-ref arr (rkt-type type) (add1 index)))
(define (sized-hakrit-array-set! arr type index value) (ptr-set! arr (rkt-type type) (add1 index) value))
(define (sized-hakrit-array-size arr) (ptr-ref arr _uint64 0))

(define (sized-hakrit-array->racket-list ptr type)
  (define size (sized-hakrit-array-size ptr))
  (define lst (cblock->list ptr (rkt-type type) (add1 size)))
  (cdr lst))

;; (define (nat-array lst)
;;   (make-fixed-hakrit-array lst 'nat))
;; (define (nat-array-ref arr index)
;;   (fixed-hakrit-array-ref arr 'nat index))
;; (define (nat-array-set! arr index val)
;;   (fixed-hakrit-array-set! arr 'nat index val))

;; (define (real-array lst)
;;   (make-fixed-hakrit-array lst 'real))
;; (define (real-array-ref arr index)
;;   (fixed-hakrit-array-ref arr 'real index))
;; (define (real-array-set! arr index val)
;;   (fixed-hakrit-array-set! arr 'real index val))


;; interpreter utils
(define (make-interp-hakrit-array val) (if (vector? val) val (apply vector val)))
(define interp-hakr-array-ref vector-ref)
