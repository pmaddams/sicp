#lang racket/base

(require racket/dict
         rackunit
         "main.rkt")

(test-case
 "sum-largest-squares"
 (for (((in want)
        (in-dict '(((1 2 3) . 13)
                   ((4 3 2) . 25)
                   ((5 3 4) . 41)))))
   (let ((got (apply sum-largest-squares in)))
     (check-equal? got want))))
