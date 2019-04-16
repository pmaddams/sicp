#lang racket/base

; Exercise 1.12

(provide (all-defined-out))

(require racket/sequence)

(define (pascal r c)
  (if (or (< c 0) (> c r))
      0
      (let loop ((r r) (c c))
        (if (or (= c 0) (= c r))
            1
            (+ (loop (sub1 r) (sub1 c))
               (loop (sub1 r) c))))))

(define (choose n k)
  (if (or (< k 0) (> k n))
      0
      (quotient (factorial (+ k 1) n)
                (factorial (- n k)))))

(define factorial
  (case-lambda
    ((n) (factorial 2 n))
    ((lo hi) (product (in-range lo (add1 hi))))))

(define (product seq) (sequence-fold * 1 seq))
