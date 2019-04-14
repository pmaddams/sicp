#lang racket/base

; Exercise 1.3

(provide (all-defined-out))

(define (sum-of-larger-squares . args)
  (let* ((n (apply min args))
         (l (remove n args)))
    (apply sum-of-squares l)))

(define (sum-of-squares . args)
  (apply + (map square args)))

(define (square n) (* n n))
