#lang racket/base

; Exercise 1.12

(provide pascal)

(require racket/sequence)

(define (pascal n)
  (for/list ((k (in-range (add1 n))))
    (choose n k)))

(define (choose n k)
  (quotient (product (add1 k) n)
            (product 2 (- n k))))

(define (product lo hi)
  (sequence-fold * 1 (in-range lo (add1 hi))))
