#lang sicp

(define (compose f g)
  (lambda (x)
    (f (g x))))

(let ((square (lambda (x)
                (expt x 2))))
  ((compose square inc) 6))
;; 49