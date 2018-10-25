#lang racket/base

(provide sum-largest-squares)

(define (sum-largest-squares . l)
  (apply + (map square (remove-smallest l))))

(define (square n) (* n n))

(define (remove-smallest l)
  (let ((smallest (apply min l)))
    (remove smallest l)))
