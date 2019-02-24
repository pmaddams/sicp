#lang racket/base

; Exercise 2.19

(provide (all-defined-out))

(define us-coins '(1 5 10 25 50))

(define uk-coins '(0.5 1 2 5 10 20 50 100))

(define (change amount coins)
  (cond ((or (negative? amount) (null? coins)) 0)
        ((zero? amount) 1)
        (else (+ (change amount (cdr coins))
                 (change (- amount (car coins)) coins)))))
