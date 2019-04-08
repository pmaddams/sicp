#lang racket/base

(require rackunit
         "main.rkt")

(test-case
 "arithmetic"
 (for ((f (in-list (list add sub mul div pow)))
       (g (in-list (list + - * quotient expt))))
   (for ((i (in-range 5)))
     (let ((n (random 1 10))
           (m (random 1 10)))
       (check-equal? (f n m) (g n m))))))
