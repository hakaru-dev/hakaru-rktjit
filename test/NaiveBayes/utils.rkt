#lang racket

(provide (all-defined-out))
(define (get-time)
  (/ (current-milliseconds) 1000.0))
(define (elasp-time from)
  (- (get-time) from))
