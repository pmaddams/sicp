#lang sicp

(define (fast-expt-iter b n)
  (letrec ((square (lambda (x)
                     (* x x)))
           (expt-iter (lambda (a b n)
                        (cond ((zero? n) a)
                              ((even? n) (expt-iter a (square b) (/ n 2)))
                              (else (expt-iter (* a b) b (dec n)))))))
    (expt-iter 1 b n)))