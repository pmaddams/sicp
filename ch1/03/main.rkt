#lang racket/base

; Exercise 1.3

(provide sum-larger-squares
         sum-of-squares
         remove-smallest)

(define (sum-larger-squares . args)
  (apply sum-of-squares (remove-smallest args)))

(define (sum-of-squares . args)
  (apply + (map square args)))

(define (remove-smallest l)
  (remove (apply min l) l))

(define (square n) (* n n))