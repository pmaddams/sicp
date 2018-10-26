#lang racket/base

(require rackunit
         "main.rkt")

(define (row n)
  (for/list ((k (in-range (add1 n))))
    (choose n k)))

(test-case
 "choose"
 (for ((n (in-range 5))
       (l '((1)
            (1 1)
            (1 2 1)
            (1 3 3 1)
            (1 4 6 4 1))))
   (check-equal? (row n) l)))
