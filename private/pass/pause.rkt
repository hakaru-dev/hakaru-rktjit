#lang racket
(require sham)
(require "ast.rkt"
         "utils.rkt")
(provide pause)

(define state (box (void)))
(define rest-pass (box (void)))

(define-namespace-anchor anchor)
(define ns (namespace-anchor->namespace anchor))

(define (pause st rp)
  (set-box! state st)
  (set-box! rest-pass rp)
  (printf "pausing compilation\n")
  (printf "\n> ")
  (let loop ([inp (read)])
    (unless (equal? inp 'continue)
       (printf "evaluating: ~a\n" inp)
       (printf "~a\n"(eval inp ns))
       (printf "\n> ")
       (loop (read))))
  (printf "continuing evaluation\n")
  ((car rp) st (cdr rp)))
