#lang racket/base

(require racket/class
         rackunit
         "main.rkt")

(test-case
 "celsius->fahrenheit"
 (let* ((c (new connector%))
        (f (celsius->fahrenheit c)))
   (set c 0)
   (check-equal? (get f) 32)
   (set c 100)
   (check-equal? (get f) 212)))
