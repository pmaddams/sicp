#lang sicp

(define (fast-expt-iter b n)
  (letrec ((square (lambda (x)
                     (* x x)))
           (f (lambda (a b n)
                        (cond ((zero? n) a)
                              ((even? n) (f a (square b) (/ n 2)))
                              (else (f (* a b) b (dec n)))))))
    (f 1 b n)))
