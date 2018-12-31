#lang racket

(require "utils.rkt"
         "ast.rkt")

(provide split-prog)

(define debug-split-prog (make-parameter #t))
(define dsp (debug-printf debug-split-prog))


(define (split-prog st)
  (define (pass p)
    (void))
  (match st
    [(state prgs info os)
     (define nprgs (pass prgs))
     (dsp "combine loops:\n~a\n" (pretty-format (pe nprgs)))
     (run-next nprgs info st)]))
