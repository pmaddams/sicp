#lang racket/base

(require rackunit
         "main.rkt")

(test-case
 "sum-of-larger-squares"
 (check-equal? (sum-of-larger-squares 1 2 3) (sum-of-squares 2 3))
 (check-equal? (sum-of-larger-squares 5 4 3 2) (sum-of-squares 5 4 3))
 (check-equal? (sum-of-larger-squares 6 2 5 3 4) (sum-of-squares 6 5 3 4)))

(test-case
 "sum-of-squares"
 (check-equal? (sum-of-squares 2 3) 13)
 (check-equal? (sum-of-squares 5 4 3) 50)
 (check-equal? (sum-of-squares 6 5 3 4) 86))
