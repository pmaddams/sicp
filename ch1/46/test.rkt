#lang racket/base

(require rackunit
         "main.rkt")

(test-case
 "sqrt-approx"
 (let* ((% 0.00001)
        (f (sqrt-approx %)))
   (for ((i (in-range 5)))
     (let ((x (random 1000000)))
       (check (within? %) (f x) (sqrt x))))))

(test-case
 "nth-root-approx"
 (for ((n (in-range 1 11)))
   (let ((x (random 1000)))
     (for ((% (in-list '(1.0 0.1 0.01))))
       (check (within? %)
              ((nth-root-approx n %) x)
              (expt x (/ 1 n)))))))

(define (square n) (* n n))

(test-case
 "repeated"
 (for ((n (in-range 5)))
   (check-equal? ((repeated add1 n) 0) n)
   (check-equal? ((repeated square n) 2) (expt 2 (expt 2 n)))))
