#lang racket/base

(require rackunit
         "main.rkt")

(define primes '(2 3 5 7 11 13 17 23 29 31))

(define composites '(0 1 4 6 8 9 10 12 14 15))

(test-case
 "prime?"
 (for ((n primes))
   (check-true (prime? n)))
 (for ((n composites))
   (check-false (prime? n))))
