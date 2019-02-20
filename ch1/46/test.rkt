#lang racket/base

(require rackunit
         "main.rkt")

(test-case
 "nth-root-approx"
 (let ((% 0.1))
   (for ((i (in-range 5)))
     (let ((n (random 2 5))
           (x (random 10 100)))
       (check (within? %)
              (((nth-root-approx %) n) x)
              (expt x (/ 1 n)))))))

(define (square n) (* n n))

(test-case
 "repeated"
 (for ((n (in-range 5)))
   (check-equal? ((repeated add1 n) 0) n)
   (check-equal? ((repeated square n) 2) (expt 2 (expt 2 n)))))
