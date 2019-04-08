#lang racket/base

(require rackunit
         "main.rkt")

(test-case
 "A"
 (for ((proc (in-list (list f g h)))
       (n (in-range 3)))
   (for ((m (in-range 5)))
     (check-equal? (A n m) (proc m)))))
