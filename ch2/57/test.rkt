#lang racket/base

(require racket/dict
         rackunit
         "main.rkt")

(define differentiated
  `((1 . 0)
    (x . 1)
    ((+ x x x) . 3)
    ((* x x x) . (* 3 x x))))

(test-case
 "differentiation"
 (for (((f g) (in-dict differentiated)))
   (for ((i (in-range 5)))
     (let* ((x (random -100 100)))
       (check-equal? (eval-deriv f 'x x)
                     (eval g 'x x))))))
