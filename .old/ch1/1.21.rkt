#lang sicp

(define (square x)
  (expt x 2))

(define (divides? a b)
  (zero? (remainder b a)))

(define (smallest-divisor n)
  (letrec ((s (lambda (i)
                (cond ((> (square i) n) n)
                      ((divides? i n) i)
                      (else (s (inc i)))))))
    (s 2)))

(smallest-divisor 199)
;; 199

(smallest-divisor 1999)
;; 1999

(smallest-divisor 19999)
;; 7
