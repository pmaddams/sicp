#lang racket/base

(require racket/dict
         rackunit
         "main.rkt")

(define derivatives
  `((1 . 0)
    (x . 1)
    ((+ x x x) . 3)
    ((* x x x) . (* 3 x x))))

(test-case
 "differentiation"
 (for (((f f-prime) (in-dict derivatives)))
   (for ((i (in-range 5)))
     (let* ((x (random -100 100)))
       (check-equal? (eval-deriv f 'x x)
                     (eval f-prime 'x x))))))
