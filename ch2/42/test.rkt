#lang racket/base

(require rackunit
         "main.rkt")

(test-case
 "queens"
 (check-equal? (for/list ((n (in-range 1 9)))
                 (length (queens n)))
               '(1 0 0 2 10 4 40 92)))
