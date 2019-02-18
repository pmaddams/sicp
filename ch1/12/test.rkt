#lang racket/base

(require rackunit
         "main.rkt")

(test-case
 "pascal"
 (for ((n (in-range 5))
       (l '((1)
            (1 1)
            (1 2 1)
            (1 3 3 1)
            (1 4 6 4 1))))
   (check-equal? (pascal n) l)))
