#lang racket/base

; Exercise 1.12: Pascal's triangle

(provide choose)

(define (choose n k)
  (if (or (zero? n) (zero? k) (= n k))
      1
      (+ (choose (sub1 n) (sub1 k))
         (choose (sub1 n) k))))