#lang sicp

(define (double f)
  (lambda (x)
    (f (f x))))

(define (repeated f n)
  (if (= n 1)
      f
      (repeated (double f) (dec n))))

(define (square x)
  (expt x 2))

((repeated square 2) 5)
;; 625
