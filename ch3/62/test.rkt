#lang racket/base

(require racket/math
         rackunit
         "main.rkt")

(test-case
 "series"
 (for ((i (in-range 5)))
   (for ((% (in-list '(0.1 0.01 0.001))))
     (let ((x (* (if (random 1) 1 -1)
                 (random)
                 (/ pi 4))))
       (for ((f (in-list (list (sin-approx %)
                               (cos-approx %)
                               (tan-approx %)
                               (exp-approx %))))
             (g (in-list (list sin cos tan exp))))
         (check (within? %) (f x) (g x)))))))
