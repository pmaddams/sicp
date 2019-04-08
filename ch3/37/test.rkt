#lang racket/base

(require racket/class
         rackunit
         "main.rkt")

(test-case
 "basic tests"
 (for ((f (in-list (list add sub mul div)))
       (g (in-list (list + - * /))))
   (for ((i (in-range 5)))
     (let ((n (random 1 10))
           (m (random 1 10)))
       (check-equal? (get (f (const n) (const m)))
                     (g n m))))))

(test-case
 "celsius->fahrenheit"
 (let* ((c (new connector%))
        (f (celsius->fahrenheit c)))
   (set c 0)
   (check-equal? (get f) 32)
   (set c 100)
   (check-equal? (get f) 212)))
