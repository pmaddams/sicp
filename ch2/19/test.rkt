#lang racket/base

(require rackunit
         "main.rkt")

(test-case
 "basic tests"
 (for ((i (in-range 5)))
   (let ((n (random 1 10)))
     (check-equal? (change n '(1)) 1)
     (check-equal? (change n '()) 0)
     (check-equal? (change n (list (add1 n))) 0))))

(test-case
 "us-coins"
 (check-equal? (change 100 us-coins) 292))
