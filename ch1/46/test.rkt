#lang racket/base

(require rackunit
         "main.rkt")

(test-case
 "nth-root-approx"
 (for ((i 5))
   (let ((n (random 2 5))
         (x (random 10 100)))
     (for ((% '(0.1 0.01 0.001)))
       (check (within? %)
              ((nth-root-approx n %) x)
              (expt x (/ 1 n)))))))

(define (square n) (* n n))

(test-case
 "repeated"
 (for ((n 5))
   (check-equal? ((repeated add1 n) 0) n)
   (check-equal? ((repeated square n) 2) (expt 2 (expt 2 n)))))
