#lang racket

(require sham/ast)

(provide (all-defined-out))
(define (new-prelude)
  (make-hash))
(define (add-defs-prelude! prelude defs)
  (map (λ (d) (hash-set! prelude (sham:def-id d) d)) defs)
  prelude)
(define (get-defs-prelude prelude)
  (hash-values prelude))

(define (cleanup-defs defs)
  (define already-seen (mutable-set))
  (for/list ([def defs]
             #:when (not (set-member? already-seen (sham:def-id def))))
    (set-add! already-seen (sham:def-id def))
    def))

