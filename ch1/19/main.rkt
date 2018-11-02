#lang racket/base

(provide fib)

(define (fib n)
  (let loop ((n n) (a 1) (b 0) (p 0) (q 1))
    (cond ((zero? n) b)
          ((even? n) (loop (/ n 2) a b (next-p p q) (next-q p q)))
          (else (loop (sub1 n) (next-a a b p q) (next-b a b p q) p q)))))

(define (next-a a b p q)
  (+ (* (+ p q) a) (* q b)))

(define (next-b a b p q)
  (+ (* q a) (* p b)))

(define (next-p p q)
  (+ (square p) (square q)))

(define (next-q p q)
  (+ (* 2 p q) (square q)))

(define (square n)
  (* n n))
