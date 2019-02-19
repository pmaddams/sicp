#lang racket/base

; Exercise 1.12

(provide pascal)

(define (pascal n)
  (for/list ((k (in-range (add1 n))))
    (choose n k)))

(define (choose n k)
  (quotient (factorial (add1 k) n)
            (factorial (- n k))))

(define factorial
  (case-lambda
    ((hi) (factorial 2 hi))
    ((lo hi) (for/product ((n (in-range lo (add1 hi)))) n))))
