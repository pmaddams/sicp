#lang racket/base

(require rackunit
         "main.rkt")

(test-case
 "A"
 (check-equal? (A 1 10) 1024)
 (check-equal? (A 2 4) 65536)
 (check-equal? (A 3 3) 65536))

(test-case
 "derived functions"
 (for ((proc (in-list (list f g h)))
       (n (in-range 3)))
   (for ((m (in-range 5)))
     (check-equal? (A n m) (proc m)))))
