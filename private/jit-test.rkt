#lang racket

(require ffi/unsafe)
(require sham/jit)
(require "jit-utils.rkt"
         "utils.rkt"
         "basic-defines.rkt"
         "jit.rkt")
(define
    benv
    (initialize-jit
     (compile-module
      `(#%module


        ,@(basic-defines)

(define-function (#:pass)
   (#:attr AlwaysInline)
   (main
    (topic_prior : array-prob-p)
    (word_prior : array-prob-p)
    (z : array-nat-p)
    (w : array-nat-p)
    (doc : array-nat-p)
    (docUpdate : nat)
    :
    array-prob-p)
   (let ((bk2a : array-array-nat-p #%void))
     (block
       (let ((arri1 : array-array-nat-p #%void))
         (block
           (set! arri1 (#%app empty-array-array-nat (#%app size-array-nat-p z)))
           (block
             (let ((fi1 : nat (#%ui-value 0 nat)))
               (let ((end1 : nat (#%app size-array-nat-p z)))
                 (while ((fi1 : nat))
                        (#%app jit-icmp-ult fi1 end1)
                   (block
                     (let ((arri2 : array-nat-p #%void))
                       (block
                         (set! arri2 (#%app empty-array-nat (#%app size-array-prob-p word_prior)))
                         (block
                           (let ((fi2 : nat (#%ui-value 0 nat)))
                             (let ((end2 : nat (#%app size-array-prob-p word_prior)))
                               (while ((fi2 : nat))
                                      (#%app jit-icmp-ult fi2 end2)
                                 (block
                                   (#%exp
                                    (#%app set-array-nat-at-index arri2 fi2 (#%ui-value 0 nat)))
                                   (set! fi2 (#%app jit-add-nuw fi2 (#%ui-value 1 nat)))))))
                           (#%exp (#%app set-array-array-nat-at-index arri1 fi1 arri2)))))
                     (set! fi1 (#%app jit-add-nuw fi1 (#%ui-value 1 nat)))))))
             (set! bk2a arri1))))
       (block
         (let ((bk2i1 : nat (#%ui-value 0 nat)))
           (let ((end3 : nat (#%app size-array-nat-p w)))
             (while ((bk2i1 : nat))
                    (#%app jit-icmp-ult bk2i1 end3)
               (block
                 (block
                   (#%exp
                    (#%app
                     set-array-nat-at-index
                     (#%app index-array-array-nat-p bk2a (#%app index-array-nat-p doc bk2i1))
                     (#%app
                      index-array-nat-p
                      w
                      (#%app index-array-array-nat-p bk2a (#%app index-array-nat-p doc bk2i1)))
                     (#%app
                      add-2-nat
                      (#%app
                       index-array-nat-p
                       (#%app index-array-array-nat-p bk2a (#%app index-array-nat-p doc bk2i1))
                       (#%app
                        index-array-nat-p
                        w
                        (#%app index-array-array-nat-p bk2a (#%app index-array-nat-p doc bk2i1))))
                      (#%ui-value 1 nat))))
                   (#%exp #%void))
                 (set! bk2i1 (#%app jit-add-nuw bk2i1 (#%ui-value 1 nat)))))))
         (return topic_prior)
         ;; (let ((ar2 : array-prob-p #%void))
         ;;   (block
         ;;     (let ((ae1 : nat #%void))
         ;;       (block
         ;;         (set! ae1 (#%app size-array-prob-p topic_prior))
         ;;         (let ((tmp1 : array-prob-p (#%app empty-array-prob ae1)))
         ;;           (let ((ai1 : nat (#%ui-value 0 nat)))
         ;;             (block
         ;;               (while ((ai1 : nat) (tmp1 : prob))
         ;;                      (#%app jit-icmp-ult ai1 ae1)
         ;;                 (let ((tmpi1 : prob (#%app real2prob (#%fl-value 0.0 real))))
         ;;                   (block
         ;;                     (set! tmpi1
         ;;                       (#%app
         ;;                        nat2prob
         ;;                        (#%app
         ;;                         index-array-nat-p
         ;;                         (#%app index-array-array-nat-p bk2a docUpdate)
         ;;                         (#%ui-value 0 nat))))
         ;;                     (#%exp (#%app set-array-prob-at-index tmp1 ai1 tmpi1))
         ;;                     (set! ai1 (#%app jit-add-nuw ai1 (#%ui-value 1 nat))))))
         ;;               (set! ar2 tmp1))))))
         ;;     (return ar2)))
         ))))
))))
;(jit-dump-module benv)
  (define (get-t t) (jit-get-racket-type (env-lookup t benv)))
  (define (get-f f) (jit-get-function f benv))
  (define t-real (get-t 'real))
  (define t-nat (get-t 'nat))
  (define t-prob (get-t 'prob))

  (define c-nat2prob (get-f 'nat2prob))
  (define c-prob2real (get-f 'prob2real))
  (define c-real2prob (get-f 'real2prob))

  (define recip-nat (get-f 'recip-nat))
  (define recip-real (get-f 'recip-real))
  (define recip-prob (get-f 'recip-prob))

  (define add-2-nat (get-f 'add-2-nat))
  (define add-2-real (get-f 'add-2-real))
  (define add-3-real (get-f 'add-3-real))
  (define add-2-prob (get-f 'add-2-prob))
  (define add-3-prob (get-f 'add-3-prob))
  
  (define mul-2-nat (get-f 'mul-2-nat))
  (define mul-2-real (get-f 'mul-2-real))
  (define mul-2-prob (get-f 'mul-2-prob))
  (define mul-4-prob (get-f 'mul-4-prob))


  ;; (define make-array-nat (get-f 'make-array-nat))
  ;; (define index-array-nat (get-f 'index-array-nat-p))
  ;; (define size-nat (get-f 'size-array-nat-p))
  ;; (define set-array-nat-at-index! (get-f 'set-array-nat-at-index))
  ;; (define empty-array-nat (get-f 'empty-array-nat))


  ;; (define make-array-array-nat (get-f 'make-array-array-nat))
  ;; (define index-array-array-nat (get-f 'index-array-array-nat-p))
  ;; (define size-array-array-nat (get-f 'size-array-array-nat-p))
  ;; (define set-array-array-nat-at-index! (get-f 'set-array-array-nat-at-index))
  ;; (define empty-array-array-nat (get-f 'empty-array-array-nat))

  ;; (define make-array-real (get-f 'make-array-real))
  ;; (define index-array-real (get-f 'index-array-real-p))
  ;; (define size-real (get-f 'size-array-real-p))
  ;; (define set-array-real-at-index! (get-f 'set-array-real-at-index))
  ;; (define empty-array-real (get-f 'empty-array-real))

  ;; (define make-array-prob (get-f 'make-array-prob))
  ;; (define index-array-prob (get-f 'index-array-prob-p))
  ;; (define size-prob (get-f 'size-array-prob-p))
  ;; (define set-array-prob-at-index! (get-f 'set-array-prob-at-index))
  ;; (define empty-array-prob (get-f 'empty-array-prob))  


(module+ test
  
  (hakaru-defines benv)
  (define main (get-f 'main))

  (define read-from-csv (compose make-c-array-nat read-vector-from-csv))
  (define topic-prior (make-c-array-prob (replicate-vector 10 1.0)))
  (define word-prior (make-c-array-prob (replicate-vector 100 1.0)))
  (define v (read-from-csv "../test/input/small-arg3.csv")) ;;size 40x0   ;; values 0-20
  (define words (read-from-csv "../test/input/small-arg4.csv")) ;;size 47049 ;; values 0-7022
  (define docs (read-from-csv "../test/input/small-arg5.csv")) ;;size 47049 ;; values 0-400
  (define docUpdate 0) ;; value 0-400
  (define result-raw (time (main topic-prior word-prior v words docs docUpdate)))
  (define result-vector
    (cblock->vector (get-array-prob result-raw) prob-type (size-array-prob result-raw)))
  (pretty-display result-vector)
  )
