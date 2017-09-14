#lang sicp

(define (foldr proc init l)
  (letrec ((f (lambda (l)
                (if (null? l)
                    init
                    (proc (car l)
                          (f (cdr l)))))))
    (f l)))

(define (horner x coeffs)
  (foldr (lambda (coeff higher-terms)
           (+ coeff
              (* x higher-terms)))
         0
         coeffs))

(horner 2 '(1 3 0 5 0 1))
;; 79
