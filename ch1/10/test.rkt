#lang racket/base

(require rackunit
         "main.rkt")

(test-case
 "ackermann"
 (check-equal? (ackermann 1 10) 1024)
 (check-equal? (ackermann 2 4) 65536)
 (check-equal? (ackermann 3 3) 65536))

(test-case
 "derived functions"
 (for ((proc (in-list (list f g h)))
       (n (in-range 3)))
   (for ((m (in-range 5)))
     (check-equal? (ackermann n m) (proc m)))))
