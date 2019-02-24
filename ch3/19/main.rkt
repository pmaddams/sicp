#lang racket/base

; Exercise 3.19

(provide (all-defined-out))

(define (has-cycle? l)
  (define (next l)
    (if (null? l) '() (cdr l)))

  (let loop ((l1 l) (l2 (next l)))
    (and (not (null? l1))
         (not (null? l2))
         (or (eq? l1 l2)
             (eq? l1 (next l2))
             (loop (next l1) (next (next l2)))))))
