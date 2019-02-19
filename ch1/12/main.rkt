#lang racket/base

; Exercise 1.12

(provide pascal)

(require racket/sequence)

(define (pascal n)
  (for/list ((k (in-range (add1 n))))
    (choose n k)))

(define (choose n k)
  (quotient (factorial (add1 k) n)
            (factorial (- n k))))

(define factorial
  (case-lambda
    ((n) (factorial 2 n))
    ((lo hi) (product (in-range lo (add1 hi))))))

(define (product seq) (sequence-fold * 1 seq))
