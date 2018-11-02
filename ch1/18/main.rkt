#lang racket/base

(provide add sub mul div pow)

(define (add n m)
  (if (zero? m)
      n
      (add (add1 n) (sub1 m))))

(define (sub n m)
  (if (zero? m)
      n
      (sub (sub1 n) (sub1 m))))

(define (mul n m)
  (let loop ((n n) (m m) (acc 0))
    (cond ((zero? m) acc)
          ((even? m) (loop (double n) (halve m) acc))
          (else (loop n (sub1 m) (add n acc))))))

(define (double n)
  (add n n))

(define (halve n)
  (div n 2))

(define (div n m)
  (let loop ((n n) (acc 0))
    (if (< n m)
        acc
        (loop (sub n m) (add1 acc)))))

(define (pow n m)
  (let loop ((n n) (m m) (acc 1))
    (cond ((zero? m) acc)
          ((even? m) (loop (square n) (halve m) acc))
          (else (loop n (sub1 m) (mul n acc))))))

(define (square n)
  (mul n n))
