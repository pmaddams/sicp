#lang racket/base

; Exercise 1.19

(provide (all-defined-out))

(define (fibonacci n)
  (let loop ((a 1) (b 0) (p 0) (q 1) (n n))
    (cond ((zero? n) b)
          ((even? n)
           (loop a b (next-p p q) (next-q p q) (/ n 2)))
          (else
           (loop (+ (* a (+ p q)) (* b q))
                 (+ (* a q) (* b p))
                 p
                 q
                 (sub1 n))))))

(define (next-p p q)
  (+ (square p) (square q)))

(define (next-q p q)
  (+ (* 2 p q) (square q)))

(define (square n) (* n n))
