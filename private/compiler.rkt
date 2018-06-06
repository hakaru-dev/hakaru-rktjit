#lang racket

(require
 ffi/unsafe
 sham
 "parse-hk.rkt"
 "ast.rkt"
 "jit.rkt"
 "utils.rkt")
(provide (all-defined-out))

(define (load-file file-name)
  (parse (file->value file-name)))

(define (add-arg-info prog-expr info)
  (define (info-size info)
    (match info
      [`(size . ,s) s]
      [`(value . ,v)
       #:when (vector? v)
       (vector-length v)]
      [`(value . ,v)
       #:when (list? v)
       (length v)]
      [else #f]))

  (match prog-expr
    [(expr-fun name args ret-type body)
     (for ([arg args])
       (match-define (expr-var type sym _) arg)
       (define inf (assocv sym info))
       (set-expr-var-type!
        arg
        (match type
          [`(array ,t)
           (if (and inf (info-size inf))
               `(array ,t (size . ,(info-size inf)))
               type)]
          [else type]))
       (when inf (set-expr-var-info! arg inf)))
     prog-expr]))

(define (old-style-prog-info prog-expr info)
  (match-define (expr-fun name args ret-type body) prog-expr)
  (for/list ([arg args])
    (match-define (expr-var type sym _) arg)
    (define inf (assocv sym info))
    (match type
      [`(array ,t) `()]
      [`(array ,t (size . ,s))
       (match info
         [`(size . ,s)
          `((array-info . ((size . ,s))))]
         [`(value . ,v)
          #:when (vector? v)
          `((array-info . ((size . ,(vector-length v))))
            (value . ,(vector->list v)))]
         [`(value . ,v)
          #:when (list? v)
          `((array-info . ((size . ,(length v))))
            (value . ,v))]
         [else '()])]
      [else '()])))
(define (compile-hakaru file-name . info)
  (define prog-expr (load-file file-name))
  (parameterize ([hakrit-print-debug #t])
    (pretty-print (print-expr prog-expr)))

  (add-arg-info prog-expr info)
  (parameterize ([hakrit-print-debug #t])
    (pretty-print (print-expr prog-expr)))
  (define sham-module (compile-function prog-expr (old-style-prog-info prog-expr info)))
  (define init-rng (get-function 'init-rng sham-module))
  (init-rng)
  ;; (wrap-prog sham-module hakrit-file info)
  ;; (jit-dump-function sham-module 'prog)
  (get-function 'prog sham-module)
  )


(define (rkt-type t)
  (match t
    ['nat _uint64]
    ['prob _double]
    ['real _double]))

(define (make-fixed-hakrit-array arr type)
  (list->cblock arr (rkt-type type)))
(define (make-sized-hakrit-array arr type)
  (define ret (list->cblock (cons (car arr) arr) (rkt-type type)))
  (ptr-set! ret _uint64 0 (length arr))
  ret)

(define (fixed-hakrit-array-ref arr type index)
  (ptr-ref arr (rkt-type type) index))
(define (fixed-hakrit-array-set! arr type index value)
  (ptr-set! arr (rkt-type type) index value))
(define (sized-hakrit-array-ref arr type index)
  (ptr-ref arr (rkt-type type) (add1 index)))
(define (sized-hakrit-array-set! arr type index value)
  (ptr-set! arr (rkt-type type) (add1 index) value))
(define (sized-hakrit-array-size arr)
  (ptr-ref arr _uint64 0))

(module+ test
  (define gmm-gibbs-file "../../testcode/hkrkt/GmmGibbs.hkr")
  (define classes 3)
  (define points 10)
  (define prog
    (compile-hakaru gmm-gibbs-file
                    `(s  . (value . 0.0))
                    `(as . (value . (0.0 0.0 0.0)))
                    `(z  . (size . 10))
                    `(t  . (value . [3.8 1.1 -0.3 2.2 0.1 2.2 0.1 1.4 2.1 1.7]))))
  (define z (make-fixed-hakrit-array '(2 2 2 2 1 2 2 2 2 2) 'nat))
  (printf "(prog ... 0) = ~a\n"
          (prog 0.0
                (make-fixed-hakrit-array '(0.0 0.0 0.0) 'prob)
                z
                (make-fixed-hakrit-array
                 '[3.8 1.1 -0.3 2.2 0.1 2.2 0.1 1.4 2.1 1.7] 'prob) 10)))
