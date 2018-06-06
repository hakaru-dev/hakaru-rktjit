#lang racket
(require sham)
(require "ast.rkt"
         "utils.rkt")
(provide pause stop debug-print debug-print-stop)

(define debug-print-stop (make-parameter #f))
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
  (match st
    [(state prg info rp)
     (if (debug-print-stop)
         (begin (pretty-display info)
                (error 'stop))
         (run-next prg info st))]))

(define (debug-print st)
  (match st
    [(state prg info os)
     (when (debug-print-stop)
       (if (list? prg)
           (if (expr? (car prg))
               (printf "debug-printing multiple: \n~a\n"
                       (map (compose pretty-format pe) prg))
               (printf "debug-printing multiple: \n~a\n"
                       (map (compose pretty-format print-sham-def) prg)))
           (printf "debug-printing: \n~a\n" (pretty-format (pe prg)))))
     (run-next prg info st)]))
