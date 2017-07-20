#lang sicp

(define (smallest-divisor n)
  (letrec ((square (lambda (x)
                     (expt x 2)))
           (divides? (lambda (a b)
                       (zero? (remainder b a))))
           (find-divisor (lambda (n test-divisor)
                           (cond ((> (square test-divisor) n) n)
                                 ((divides? test-divisor n) test-divisor)
                                 (else (find-divisor n (inc test-divisor)))))))
    (find-divisor n 2)))

(smallest-divisor 199)
;; 199

(smallest-divisor 1999)
;; 1999

(smallest-divisor 19999)
;; 7