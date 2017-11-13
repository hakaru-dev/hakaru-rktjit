#lang racket
(require sham)
(require "ast.rkt"
         "utils.rkt")
(provide pause stop)

(define state-box (box (void)))

(define-namespace-anchor anchor)
(define ns (namespace-anchor->namespace anchor))

(define (pause st)
  (set-box! state-box st)
  (printf "pausing compilation\n")
  (printf "\n> ")
  (let loop ([inp (read)])
    (unless (equal? inp 'continue)
       (printf "evaluating: ~a\n" inp)
       (printf "~a\n"(eval inp ns))
       (printf "\n> ")
       (loop (read))))
  (printf "continuing evaluation\n")
  (run-next-state st))

(define (stop st)
  (printf "stopping pipeline here")
  (match st
    [(state prg info rp)
     (state #f info '())]))
