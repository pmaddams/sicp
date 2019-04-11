#lang racket/base

(require rackunit
         "main.rkt")

(test-case
 "factorial"
 (for ((n (in-range 10)))
   (check-equal? (factorial n)
                 (for/product ((i (in-range 1 (add1 n)))) i))))

(test-case
 "fibonacci"
 (check-equal? (for/list ((n (in-range 10))) (fibonacci n))
               '(0 1 1 2 3 5 8 13 21 34)))
