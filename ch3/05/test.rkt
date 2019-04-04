#lang racket/base

(require racket/math
         rackunit
         "main.rkt")

(test-case
 "pi-approx"
 (for ((i (in-range 10)))
   (check-within (pi-approx 200000) pi 0.01)))
