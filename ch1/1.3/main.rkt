#lang racket/base

(provide sum-largest-squares)

(module+ main
  (for ((l '((1 2 3)
             (4 3 2)
             (5 3 4))))
    (displayln (apply sum-largest-squares l))))

(define (sum-largest-squares . l)
  (apply + (map square (remove-smallest l))))

(define (square n) (* n n))

(define (remove-smallest l)
  (let ((smallest (apply min l)))
    (remove smallest l)))
