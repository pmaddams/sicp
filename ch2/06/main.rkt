#lang racket/base

; Exercise 2.6

(provide (all-defined-out))

(require racket/function)

(define (zero f) identity)

(define (one f) f)

(define (two f) (compose f f))

(define ((number->church n) f) (repeated f n))

(define (church->number c) ((c add1) 0))

(define (church=? c1 c2)
  (= (church->number c1)
     (church->number c2)))

(define ((add c1 c2) f) (compose (c1 f) (c2 f)))

(define ((mul c1 c2) f) ((compose c1 c2) f))

(define ((pow c1 c2) f) ((c2 c1) f))

(define (repeated f n)
  (for/fold ((acc identity))
            ((i (in-range n)))
    (compose f acc)))
