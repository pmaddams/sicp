#lang racket/base

(require rackunit
         "main.rkt")

(test-case
 "nth-root-within"
 (let ((percent 0.1))
   (for ((i (in-range 5)))
     (let ((n (random 2 5))
           (x (random 10 100)))
       (check (within? percent)
              ((nth-root-within percent n) x)
              (expt x (/ 1 n)))))))
