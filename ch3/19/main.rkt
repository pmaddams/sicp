#lang racket/base

; Exercise 3.19

(provide (all-defined-out))

(define (has-cycle? ml)
  (define (next ml)
    (if (null? ml)
        '()
        (mcdr ml)))

  (let loop ((ml1 ml) (ml2 (next ml)))
    (and (not (null? ml1))
         (not (null? ml2))
         (or (eq? ml1 ml2)
             (eq? ml1 (next ml2))
             (loop (next ml1) (next (next ml2)))))))
