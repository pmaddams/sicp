#lang racket/base

(require racket/math
         rackunit
         "main.rkt")

(test-case
 "e-approx"
 (let ((e (exp 1)))
   (for ((% (in-list '(0.1 0.01 0.001))))
     (check (within? %) (e-approx %) e))))
