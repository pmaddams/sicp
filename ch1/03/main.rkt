#lang racket/base

; Exercise 1.3: Sum of squares

(provide sum-largest-squares
         sum-of-squares
         remove-smallest)

(define (sum-largest-squares . args)
  (apply sum-of-squares (remove-smallest args)))

(define (sum-of-squares . args)
  (apply + (map square args)))

(define (square n) (* n n))

(define (remove-smallest l)
  (let ((smallest (apply min l)))
    (remove smallest l)))
