#lang racket/base

(require racket/math
         rackunit
         "main.rkt")

(test-case
 "pi-approx"
 (for ((i (in-range 5)))
   (check-within (pi-approx 1000000) pi 0.01)))
