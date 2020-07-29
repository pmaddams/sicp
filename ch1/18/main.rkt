#lang racket/base

; Exercise 1.18

(provide (all-defined-out))

(define (add n m)
  (if (zero? m)
      n
      (add (add1 n) (sub1 m))))

(define (sub n m)
  (if (zero? m)
      n
      (sub (sub1 n) (sub1 m))))

(define (mul n m)
  (let loop ((acc 0) (n n) (m m))
    (cond ((zero? m) acc)
          ((even? m) (loop acc (double n) (halve m)))
          (else (loop (add acc n) n (sub1 m))))))

(define (div n m)
  (let loop ((acc 0) (n n))
    (if (< n m)
        acc
        (loop (add1 acc) (sub n m)))))

(define (pow n m)
  (let loop ((acc 1) (n n) (m m))
    (cond ((zero? m) acc)
          ((even? m) (loop acc (square n) (halve m)))
          (else (loop (mul n acc) n (sub1 m))))))

(define (double n) (add n n))

(define (halve n) (div n 2))

(define (square n) (mul n n))
