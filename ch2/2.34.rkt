#lang sicp

(define (accumulate proc init seq)
  (letrec ((a (lambda (seq)
                (if (null? seq)
                    init
                    (proc (car seq)
                          (a (cdr seq)))))))
    (a seq)))

(define (horner x coeff-seq)
  (accumulate (lambda (coeff higher-terms)
                (+ coeff (* x higher-terms)))
              0
              coeff-seq))

(horner 2 '(1 3 0 5 0 1))
;; 79
