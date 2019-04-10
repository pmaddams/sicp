#lang racket/base

(require racket/math
         rackunit
         "main.rkt")

(test-case
 "series"
 (for ((i (in-range 5)))
   (let ((x (* (if (random 1) 1 -1)
               (random)
               (/ pi 3))))
     (for ((f (in-list (list (sin-approx 10)
                             (cos-approx 10)
                             (tan-approx 10)
                             (exp-approx 10))))
           (g (in-list (list sin cos tan exp))))
       (check-= (f x) (g x) 0.03)))))
