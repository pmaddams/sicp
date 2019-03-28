#lang racket/base

; Exercise 1.3

(provide (all-defined-out))

(define (sum-of-larger-squares . args)
  (let* ((smallest (apply min args))
         (larger (remove smallest args)))
    (apply sum-of-squares larger)))

(define (sum-of-squares . args)
  (apply + (map square args)))

(define (square n) (* n n))
