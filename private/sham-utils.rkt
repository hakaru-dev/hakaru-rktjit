#lang racket
(require sham/jit
         sham/ast)

(provide (all-defined-out))

;; ;;ret-type: symbol
;; ;;rator: symbol
;; ;;rand-types: (list symbol)
;; ;;returns (list format-str args ...)
;; (define (get-rator-template ret-type rator rand-types)
;;   (match rator
;;     ['+ (list add-fun-format (length rand-types) (get-type-sym (car rand-types)))]
;;     ['* (list mul-fun-format (length rand-types) (get-type-sym (car rand-types)))]
;;     ['recip (list recip-fun-format (get-type-sym (car rand-types)))]
;;     ['empty (list make-array-fun-format (get-type-sym ret-type))]
;;     ['size (list get-array-size-fun-format (get-type-sym (car rand-types)))]
;;     ['index (list get-index-fun-format (get-type-sym (car rand-types)))]
;;     ['cons (list make-pair-fun-format (get-type-sym ret-type))]
;;     ['car (list pair-car-fun-format (get-type-sym (car rand-types)))]
;;     ['cdr (list pair-cdr-fun-format (get-type-sym (car rand-types)))]
;;     ['categorical (list categorical-fun-format (length rand-types))]))

