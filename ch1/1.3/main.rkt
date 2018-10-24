#lang racket/base

(define (sum-largest-squares . l)
  (apply sum-squares (remove-smallest l)))

(define (sum-squares . l)
  (apply + (map square l)))

(define (square n) (* n n))

(define (remove-smallest l)
  (let ((smallest (apply min l)))
    (remove smallest l)))
