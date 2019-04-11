#lang racket/base

(require rackunit
         "main.rkt")

(test-case
 "fibonacci"
 (check-equal? (for/list ((n (in-range 10))) (fibonacci n))
               '(0 1 1 2 3 5 8 13 21 34)))
