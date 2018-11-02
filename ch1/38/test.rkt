#lang racket/base

(require racket/math
         rackunit
         "main.rkt")

(test-case
 "cont-frac-within"
 (let ((e (exp 1)))
   (for ((percent '(0.1 0.01 0.001)))
     (check (within? percent) (e-within percent) e)
     (check (within? percent) (pi-within percent) pi))))
