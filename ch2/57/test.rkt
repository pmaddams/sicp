#lang racket/base

(require rackunit
         racket/dict
         "main.rkt")

(define differentiated
  `((1 . 0)
    (x . 1)
    ((+ x x) . 2)
    ((* x x) . (* 2 x))
    ((* 2 (expt x 3)) . (* 6 (expt x 2)))))

(test-case
 "differentiation"
 (for (((f g) (in-dict differentiated)))
   (for ((i (in-range 5)))
     (let* ((x (random -100 100)))
       (check-equal? (eval-deriv f 'x x)
                     (eval g 'x x))))))
