#lang racket

(require sham/ast)

(provide (all-defined-out))
;;TODO make this assoc
(define (new-prelude)
  (box '()))

(define (add-defs-prelude! prelude defs)
  (set-box! prelude (append (unbox prelude) defs))
  prelude)

(define (get-defs-prelude prelude)
  (unbox prelude))

(define (cleanup-defs defs)
  (define already-seen (mutable-set))
  (for/list ([def defs]
             #:when (not (set-member? already-seen (sham:def-id def))))
    (set-add! already-seen (sham:def-id def))
    def))
