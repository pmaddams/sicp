#lang racket/base

(require rackunit
         "main.rkt")

(test-case
 "basic tests"
 (for ((i (in-range 5)))
   (let ((n (random 1 10)))
     (check-equal? (ways-to-make-change n '(1)) 1)
     (check-equal? (ways-to-make-change n '()) 0)
     (check-equal? (ways-to-make-change n (list (add1 n))) 0))))

(test-case
 "us-coins"
 (check-equal? (ways-to-make-change 100 us-coins) 292))
