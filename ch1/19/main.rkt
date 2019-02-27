#lang racket/base

; Exercise 1.19

(provide (all-defined-out))

(define (fibonacci n)
  (let loop ((n n) (a 1) (b 0) (p 0) (q 1))
    (cond ((zero? n) b)
          ((even? n)
           (loop (/ n 2) a b (next-p p q) (next-q p q)))
          (else
           (loop (sub1 n)
                 (+ (* (+ p q) a) (* q b))
                 (+ (* q a) (* p b))
                 p
                 q)))))

(define (next-p p q)
  (+ (square p) (square q)))

(define (next-q p q)
  (+ (* 2 p q) (square q)))

(define (square n) (* n n))
